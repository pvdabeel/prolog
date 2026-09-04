/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> JOBSERVER
Local job server for parallel build execution.

Manages a pool of worker threads that pull jobs from a shared queue and
post results back. The builder submits jobs grouped by plan step and
collects results with a display callback.

Transport: in standalone mode, jobs and results flow through SWI-Prolog
message queues (thread_send_message/thread_get_message). The API is
designed so that a future transport layer (IPC or HTTP) can front the
same interface to accept remote workers.
*/

:- module(jobserver, []).

% =============================================================================
%  Queue management
% =============================================================================

:- dynamic jobserver:queue_created/1.

%! jobserver:ensure_queues is det.
%
% Create the job and result message queues (idempotent).

jobserver:ensure_queues :-
  ( jobserver:queue_created(true) -> true
  ; message_queue_create(build_jobs),
    message_queue_create(build_results),
    assertz(jobserver:queue_created(true))
  ).


%! jobserver:post_job(+Job) is det.
%
% Enqueue a build job.

jobserver:post_job(Job) :-
  thread_send_message(build_jobs, Job).


%! jobserver:get_job(-Job) is det.
%
% Dequeue a build job (blocks until one is available).

jobserver:get_job(Job) :-
  thread_get_message(build_jobs, Job).


%! jobserver:post_result(+Job, +Result) is det.
%
% Post a completed job result.

jobserver:post_result(Job, Result) :-
  thread_send_message(build_results, result(Job, Result)).


%! jobserver:get_result(-Job, -Result) is det.
%
% Collect a result (blocks until one is available).

jobserver:get_result(Job, Result) :-
  thread_get_message(build_results, result(Job, Result)).


% =============================================================================
%  Worker pool
% =============================================================================

%! jobserver:init(+NumWorkers, +Executor) is det.
%
% Create queues and spawn NumWorkers threads. Each thread runs a poll
% loop calling Executor for every job it picks up.
% Executor is a goal with signature call(Executor, Job, Slot, Result).

jobserver:init(NumWorkers, Executor) :-
  jobserver:ensure_queues,
  jobserver:spawn_workers(NumWorkers, Executor).


%! jobserver:spawn_workers(+N, +Executor) is det.
%
% Before creating `build_worker_I`, reaps a thread of that alias that
% already terminated but was never joined, so a pool torn down
% abnormally in this process does not make thread_create/3 fail with a
% permission error on the alias. A still-running thread of that alias is
% a genuine bug and is left for thread_create/3 to report.

jobserver:spawn_workers(N, Executor) :-
  forall(between(1, N, I),
    ( atom_concat(build_worker_, I, Alias),
      jobserver:reap_stale_worker(Alias),
      thread_create(jobserver:worker_loop(I, Executor), _, [alias(Alias)])
    )).


%! jobserver:reap_stale_worker(+Alias) is det.
%
% Joins a terminated, not yet joined thread named Alias; no-op otherwise.

jobserver:reap_stale_worker(Alias) :-
  ( catch(thread_property(Alias, status(Status)), _, fail),
    Status \== running
  -> catch(thread_join(Alias, _), _, true)
  ;  true
  ).


%! jobserver:worker_loop(+Slot, +Executor) is det.
%
% Poll loop: get a job, execute it, post the result. The Slot index
% identifies this worker for display purposes. Exits when it receives
% the atom 'done'.

jobserver:worker_loop(Slot, Executor) :-
  repeat,
    jobserver:get_job(Job),
    ( Job == done
    -> !
    ; jobserver:wait_for_load_average,
      jobserver:job_line_offset(Job, LineOff),
      ( catch(
          call(Executor, Job, Slot, Result),
          Error,
          ( jobserver:log_worker_error(Job, Error),
            % Synthesise a properly-typed result so jobserver:collect
            % can dispatch it as a failure; otherwise the bare
            % `error(...)` term would not match `result(_,_)` and the
            % outcome would be silently dropped (no FAIL line printed,
            % no slot_outcome recorded, no exit-code propagation).
            Result = result(LineOff, failed(exception(Error)))
          )
        )
      -> jobserver:post_result(Job, Result)
      ;  jobserver:log_worker_error(Job, executor_failed),
         jobserver:post_result(Job, result(LineOff, failed(executor_failed)))
      ),
      fail
    ).


%! jobserver:job_line_offset(+Job, -LineOff) is det.
%
% Extract the slot/line-offset from a slotted/7 job so the worker can
% synthesise a properly-typed `result(LineOff, _)` even when the
% executor never gets a chance to bind it (because of an exception or
% an unexpected failure). LineOff is `unknown` for shapes we don't
% recognise.

jobserver:job_line_offset(slotted(LineOff, _, _, _, _, _, _), LineOff) :- !.
jobserver:job_line_offset(_, unknown).


%! jobserver:log_worker_error(+Job, +Error) is det.
%
% Print a worker-side error to stderr so silent-drop bugs become
% visible. We deliberately do not raise: the caller has its own
% recovery path and we don't want to crash the worker pool.

jobserver:log_worker_error(Job, Error) :-
  ( Job = slotted(_, _, _, _, _, rule(Target:Action?{_}, _), _)
  -> format(user_error,
            '[jobserver worker error] action=~w target=~w error=~q~n',
            [Action, Target, Error])
  ;  format(user_error,
            '[jobserver worker error] job=~q error=~q~n',
            [Job, Error])
  ).


%! jobserver:wait_for_load_average is det.
%
% When --load-average is set, blocks until the 1-minute system load
% average drops below the configured threshold. Polls every 5 seconds.

jobserver:wait_for_load_average :-
  ( config:cli_load_average(Limit)
  -> jobserver:wait_until_load_below(Limit)
  ;  true
  ).

jobserver:wait_until_load_below(Limit) :-
  os:current_load_average(Load),
  ( Load < Limit
  -> true
  ;  sleep(5),
     jobserver:wait_until_load_below(Limit)
  ).


%! jobserver:shutdown(+NumWorkers) is det.
%
% Send a 'done' sentinel for each worker, then join all worker threads.
%
% Also safe to call from an exception cleanup path (builder:run_plan/6)
% where a step was abandoned midway: jobs still queued are discarded
% first so no worker starts new work, workers already executing a job
% finish it (the join waits for them, exactly as the normal collect
% would), and results nobody collected are drained afterwards so they
% cannot be mistaken for the next run's results. On the normal path
% both queues are already empty and the drains are no-ops.

jobserver:shutdown(NumWorkers) :-
  jobserver:drain_queue(build_jobs),
  forall(between(1, NumWorkers, _), jobserver:post_job(done)),
  forall(between(1, NumWorkers, I),
    ( atom_concat(build_worker_, I, Alias),
      ( catch(thread_join(Alias, _), _, true) -> true ; true )
    )),
  jobserver:drain_queue(build_results).


%! jobserver:drain_queue(+Queue) is det.
%
% Discard every message currently in Queue without blocking.

jobserver:drain_queue(Queue) :-
  ( catch(thread_get_message(Queue, _, [timeout(0)]), _, fail)
  -> jobserver:drain_queue(Queue)
  ;  true
  ).


% =============================================================================
%  Step-oriented helpers
% =============================================================================

%! jobserver:submit(+Jobs) is det.
%
% Post a list of jobs to the queue.

jobserver:submit(Jobs) :-
  forall(member(J, Jobs), jobserver:post_job(J)).


%! jobserver:collect(+Expected, +Callback) is det.
%
% Collect Expected results, calling Callback for each.
% Callback signature: call(Callback, Slot, Result).

jobserver:collect(0, _Callback) :- !.

jobserver:collect(Remaining, Callback) :-
  jobserver:get_result(_Job, Result),
  ( Result = result(Slot, Outcome)
  -> call(Callback, Slot, Outcome)
  ;  % Historically this branch just logged and continued, which meant
     % a malformed result silently disappeared from the failure tally:
     % the per-step counter decremented but no slot_outcome/2 fact was
     % asserted, so tally_outcomes/6 saw no failure even though one
     % had clearly occurred. Now we synthesise a typed failure outcome
     % and feed it through the callback so it lands in slot_outcome
     % and gets counted. The `dropped` slot key is a non-integer
     % sentinel that cannot collide with real LineOffsets.
     format(user_error,
            '[jobserver collect] dropped malformed result, recording as failure: ~q~n',
            [Result]),
     catch(call(Callback, dropped, failed(malformed_result(Result))), _, true)
  ),
  R1 is Remaining - 1,
  jobserver:collect(R1, Callback).
