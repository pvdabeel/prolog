/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

// Native Unix-domain IPC client for a portage-ng daemon.
// Functional parity with Source/Application/Client/ipclient.pl:
//   request streaming, env forward, --status, --cmd halt|relaunch,
//   --shell reject, autostart via portage-ng-dev --mode daemon --background.
//
// Wire protocol (same as ipc.pl / ipclient.pl):
//   client -> daemon:  request([Args...], Cols, Rows, [Name-Value, ...]).
//   daemon -> client:  <stdout/stderr UTF-8 stream> + NUL "EXIT:<code>\n"

#include <cerrno>
#include <climits>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/un.h>
#include <unistd.h>

namespace {

constexpr char kSentinel[] = "EXIT:";

const char* kForwardedEnv[] = {
    "USE",
    "ACCEPT_KEYWORDS",
    "ACCEPT_LICENSE",
    "PYTHON_TARGETS",
    "PYTHON_SINGLE_TARGET",
    "RUBY_TARGETS",
    "RUBY_SINGLE_TARGET",
    "LUA_SINGLE_TARGET",
    "PERL_FEATURES",
    "LLVM_SLOT",
    "VIDEO_CARDS",
    "INPUT_DEVICES",
    "CPU_FLAGS_X86",
    "APACHE2_MODULES",
    "APACHE2_MPMS",
};

std::string g_argv0;

std::string runtime_dir() {
  if (const char* xdg = std::getenv("XDG_RUNTIME_DIR"); xdg && *xdg)
    return xdg;
  const char* user = std::getenv("USER");
  if (!user || !*user)
    user = "unknown";
  return std::string("/tmp/portage-ng-") + user;
}

std::string socket_path() { return runtime_dir() + "/portage-ng.sock"; }

std::string pid_path() { return runtime_dir() + "/portage-ng.pid"; }

bool file_exists(const std::string& path) {
  return access(path.c_str(), F_OK) == 0;
}

// Always-quoted Prolog atom (~q-compatible; always valid for read_term).
std::string quote_atom(const std::string& s) {
  std::string out;
  out.reserve(s.size() + 2);
  out.push_back('\'');
  for (unsigned char c : s) {
    if (c == '\'')
      out.push_back('\'');
    out.push_back(static_cast<char>(c));
  }
  out.push_back('\'');
  return out;
}

// Same defaults as config:local_tty_size/2 / ipclient (80 rows, 160 cols).
void tty_size(int& cols, int& rows) {
  cols = 160;
  rows = 80;
  winsize ws{};
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0 &&
      ws.ws_row > 0) {
    cols = ws.ws_col;
    rows = ws.ws_row;
  }
}

std::string build_env_term() {
  std::string env = "[";
  bool first = true;
  auto add = [&](const std::string& name, const std::string& value) {
    if (!first)
      env.push_back(',');
    first = false;
    env += quote_atom(name);
    env.push_back('-');
    env += quote_atom(value);
  };
  if (isatty(STDOUT_FILENO))
    add("_CLIENT_IS_TTY", "true");
  for (const char* name : kForwardedEnv) {
    if (const char* val = std::getenv(name); val)
      add(name, val);
  }
  env.push_back(']');
  return env;
}

std::string build_request(int argc, char** argv) {
  std::string args = "[";
  for (int i = 1; i < argc; ++i) {
    if (i > 1)
      args.push_back(',');
    args += quote_atom(argv[i]);
  }
  args.push_back(']');

  int cols = 160, rows = 80;
  tty_size(cols, rows);

  std::string req = "request(";
  req += args;
  req += ", ";
  req += std::to_string(cols);
  req += ", ";
  req += std::to_string(rows);
  req += ", ";
  req += build_env_term();
  req += ").\n";
  return req;
}

int connect_socket(const std::string& path) {
  int fd = ::socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) {
    std::cerr << "Error: socket(): " << std::strerror(errno) << '\n';
    return -1;
  }
  sockaddr_un addr{};
  addr.sun_family = AF_UNIX;
  if (path.size() >= sizeof(addr.sun_path)) {
    std::cerr << "Error: socket path too long: " << path << '\n';
    ::close(fd);
    return -1;
  }
  std::memcpy(addr.sun_path, path.c_str(), path.size() + 1);
  if (::connect(fd, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) < 0) {
    std::cerr << "Error: No daemon running (socket " << path
              << " not found).\n"
              << "Start one with: portage-ng --mode daemon\n";
    ::close(fd);
    return -1;
  }
  return fd;
}

bool write_all(int fd, const std::string& data) {
  const char* p = data.data();
  size_t left = data.size();
  while (left > 0) {
    ssize_t n = ::write(fd, p, left);
    if (n < 0) {
      if (errno == EINTR)
        continue;
      std::cerr << "Error: write(): " << std::strerror(errno) << '\n';
      return false;
    }
    p += n;
    left -= static_cast<size_t>(n);
  }
  return true;
}

// Stream until NUL EXIT:<code>\n. Returns daemon exit code, or 1 on error.
int stream_response(int fd) {
  std::string carry;
  char buf[8192];

  auto emit = [](const char* data, size_t n) {
    if (n == 0)
      return;
    fwrite(data, 1, n, stdout);
    fflush(stdout);
  };

  auto parse_code = [](const std::string& digits) -> int {
    if (digits.empty())
      return 1;
    char* end = nullptr;
    long v = std::strtol(digits.c_str(), &end, 10);
    if (end == digits.c_str() || *end != '\0')
      return 1;
    return static_cast<int>(v);
  };

  auto after_nul = [&](std::string tail) -> int {
    for (;;) {
      if (tail.size() >= sizeof(kSentinel) - 1 &&
          tail.compare(0, sizeof(kSentinel) - 1, kSentinel) == 0) {
        std::string rest = tail.substr(sizeof(kSentinel) - 1);
        for (;;) {
          auto nl = rest.find('\n');
          if (nl != std::string::npos)
            return parse_code(rest.substr(0, nl));
          ssize_t n = ::read(fd, buf, sizeof(buf));
          if (n < 0) {
            if (errno == EINTR)
              continue;
            return 1;
          }
          if (n == 0)
            return parse_code(rest);
          rest.append(buf, static_cast<size_t>(n));
        }
      }
      bool maybe_prefix = false;
      for (size_t len = 1; len < sizeof(kSentinel) - 1; ++len) {
        if (tail.size() >= len &&
            tail.compare(0, len, kSentinel, len) == 0 &&
            len == tail.size()) {
          maybe_prefix = true;
          break;
        }
      }
      if (maybe_prefix || tail.empty()) {
        ssize_t n = ::read(fd, buf, sizeof(buf));
        if (n < 0) {
          if (errno == EINTR)
            continue;
          return 1;
        }
        if (n == 0) {
          emit("\0", 1);
          emit(tail.data(), tail.size());
          std::cerr
              << "Error: daemon closed the connection without an exit status.\n";
          return 1;
        }
        tail.append(buf, static_cast<size_t>(n));
        continue;
      }
      emit("\0", 1);
      carry = std::move(tail);
      return -2;
    }
  };

  for (;;) {
    ssize_t n = ::read(fd, buf, sizeof(buf));
    if (n < 0) {
      if (errno == EINTR)
        continue;
      std::cerr << "Error: read(): " << std::strerror(errno) << '\n';
      return 1;
    }
    if (n == 0) {
      emit(carry.data(), carry.size());
      std::cerr
          << "Error: daemon closed the connection without an exit status.\n";
      return 1;
    }
    carry.append(buf, static_cast<size_t>(n));
    for (;;) {
      auto nul = carry.find('\0');
      if (nul == std::string::npos) {
        emit(carry.data(), carry.size());
        carry.clear();
        break;
      }
      emit(carry.data(), nul);
      std::string tail = carry.substr(nul + 1);
      carry.clear();
      int code = after_nul(std::move(tail));
      if (code == -2)
        continue;
      return code;
    }
  }
}

void wait_for_file(const std::string& path, int retries) {
  for (int i = 0; i < retries; ++i) {
    if (file_exists(path))
      return;
    usleep(100000); // 100ms
  }
}

void wait_for_socket_gone(const std::string& path, int retries) {
  for (int i = 0; i < retries; ++i) {
    if (!file_exists(path))
      return;
    usleep(100000);
  }
}

bool autostart_enabled() {
  const char* v = std::getenv("PORTAGE_NG_IPC_AUTOSTART");
  if (!v)
    return true;
  return std::strcmp(v, "0") != 0 && std::strcmp(v, "false") != 0;
}

// Resolve portage-ng-dev: PORTAGE_NG_DEV, else sibling of this binary.
std::string daemon_wrapper() {
  if (const char* env = std::getenv("PORTAGE_NG_DEV"); env && *env) {
    if (access(env, X_OK) == 0)
      return env;
  }
  std::string base = g_argv0.empty() ? "." : g_argv0;
  auto slash = base.rfind('/');
  std::string dir = (slash == std::string::npos) ? "." : base.substr(0, slash);
  std::string cand = dir + "/portage-ng-dev";
  if (access(cand.c_str(), X_OK) == 0)
    return cand;
  // realpath of argv0 when invoked via PATH-relative name
  char resolved[PATH_MAX];
  if (!g_argv0.empty() && realpath(g_argv0.c_str(), resolved)) {
    std::string abs(resolved);
    slash = abs.rfind('/');
    dir = (slash == std::string::npos) ? "." : abs.substr(0, slash);
    cand = dir + "/portage-ng-dev";
    if (access(cand.c_str(), X_OK) == 0)
      return cand;
  }
  return {};
}

// Run portage-ng-dev --mode daemon --background; stdout/stderr discarded
// (piping both while waiting invites deadlock — same as ipclient.pl).
int start_daemon_background() {
  const std::string wrapper = daemon_wrapper();
  if (wrapper.empty()) {
    std::cerr
        << "Error: cannot locate portage-ng-dev to start the daemon.\n"
        << "Set PORTAGE_NG_DEV or start one with: portage-ng --mode daemon\n";
    return 1;
  }
  std::cout << "Starting daemon in background...\n";
  pid_t pid = fork();
  if (pid < 0) {
    std::cerr << "Error: fork(): " << std::strerror(errno) << '\n';
    return 1;
  }
  if (pid == 0) {
    int devnull = open("/dev/null", O_RDWR);
    if (devnull >= 0) {
      dup2(devnull, STDIN_FILENO);
      dup2(devnull, STDOUT_FILENO);
      dup2(devnull, STDERR_FILENO);
      if (devnull > STDERR_FILENO)
        close(devnull);
    }
    execl(wrapper.c_str(), wrapper.c_str(), "--mode", "daemon", "--background",
         static_cast<char*>(nullptr));
    _exit(127);
  }
  int status = 0;
  while (waitpid(pid, &status, 0) < 0) {
    if (errno != EINTR) {
      std::cerr << "Error: waitpid(): " << std::strerror(errno) << '\n';
      return 1;
    }
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    std::cerr << "Error: failed to start daemon (status " << status << ").\n";
    return 1;
  }
  const std::string sock = socket_path();
  wait_for_file(sock, 600);
  if (file_exists(sock)) {
    std::cout << "Daemon ready.\n";
    return 0;
  }
  std::cerr << "Warning: daemon may not have started.\n";
  return 0;
}

int maybe_autostart() {
  if (file_exists(socket_path()))
    return 0;
  if (!autostart_enabled())
    return 0;
  return start_daemon_background();
}

int cmd_status() {
  const std::string sock = socket_path();
  const std::string pidf = pid_path();
  if (!file_exists(sock)) {
    std::cout << "No daemon running.\n";
    return 1;
  }
  std::ifstream in(pidf);
  std::string pid;
  if (in && std::getline(in, pid) && !pid.empty()) {
    while (!pid.empty() && (pid.back() == '\n' || pid.back() == ' '))
      pid.pop_back();
    std::cout << "Daemon running (PID " << pid << ", socket " << sock << ")\n";
  } else {
    std::cout << "Daemon socket exists (" << sock << ") but no PID file.\n";
  }
  return 0;
}

int cmd_halt() {
  const std::string sock = socket_path();
  if (!file_exists(sock)) {
    std::cerr << "No daemon running.\n";
    return 0; // match ipclient: print error, do not fail the process hard
  }
  int fd = connect_socket(sock);
  if (fd < 0)
    return 1;
  if (!write_all(fd, "shutdown.\n")) {
    ::close(fd);
    return 1;
  }
  char buf[256];
  while (::read(fd, buf, sizeof(buf)) > 0) {
  }
  ::close(fd);
  std::cout << "Daemon stopped.\n";
  return 0;
}

int cmd_relaunch() {
  cmd_halt();
  sleep(1);
  wait_for_socket_gone(socket_path(), 50);
  return start_daemon_background();
}

int send_command(const std::string& cmd) {
  if (cmd == "halt")
    return cmd_halt();
  if (cmd == "relaunch")
    return cmd_relaunch();
  std::cerr << "Unknown command: " << cmd << ". Use halt or relaunch.\n";
  return 1;
}

void usage(const char* argv0) {
  std::cerr
      << "Usage:\n"
      << "  " << argv0 << " --mode ipc --pretend <pkg>\n"
      << "  " << argv0 << " --status\n"
      << "  " << argv0 << " --cmd halt|relaunch\n"
      << "\n"
      << "Socket: " << socket_path() << "\n";
}

// Parse --cmd NAME or --cmd=NAME from argv. Returns empty if absent.
std::string find_cmd(int argc, char** argv) {
  for (int i = 1; i < argc; ++i) {
    if (std::strcmp(argv[i], "--cmd") == 0) {
      if (i + 1 < argc)
        return argv[i + 1];
      return {};
    }
    if (std::strncmp(argv[i], "--cmd=", 6) == 0)
      return argv[i] + 6;
  }
  return {};
}

bool has_flag(int argc, char** argv, const char* flag) {
  for (int i = 1; i < argc; ++i) {
    if (std::strcmp(argv[i], flag) == 0)
      return true;
  }
  return false;
}

} // namespace

int main(int argc, char** argv) {
  g_argv0 = argc > 0 ? argv[0] : "";

  if (argc < 2) {
    usage(argv[0]);
    return 1;
  }

  if (has_flag(argc, argv, "--shell")) {
    std::cerr << "Error: --shell is not supported in ipc mode. "
                 "Use --mode standalone --shell instead.\n";
    return 1;
  }
  if (has_flag(argc, argv, "-h") || has_flag(argc, argv, "--help")) {
    usage(argv[0]);
    return 0;
  }
  if (has_flag(argc, argv, "--status"))
    return cmd_status();

  const std::string cmd = find_cmd(argc, argv);
  if (!cmd.empty())
    return send_command(cmd);
  // Bare --cmd without value
  for (int i = 1; i < argc; ++i) {
    if (std::strcmp(argv[i], "--cmd") == 0) {
      std::cerr << "Error: --cmd requires an argument\n";
      return 1;
    }
  }

  if (maybe_autostart() != 0)
    return 1;

  const std::string sock = socket_path();
  if (!file_exists(sock)) {
    std::cerr << "Error: No daemon running (socket " << sock
              << " not found).\n"
              << "Start one with: portage-ng --mode daemon\n";
    return 1;
  }

  int fd = connect_socket(sock);
  if (fd < 0)
    return 1;

  const std::string req = build_request(argc, argv);
  if (!write_all(fd, req)) {
    ::close(fd);
    return 1;
  }

  const int code = stream_response(fd);
  ::close(fd);
  return code;
}
