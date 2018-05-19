SUDO=sudo
TARGET=portage-ng

all:
	swipl --stack_limit=4G -O -q -f portage-ng.pl -g main --stand_alone=true -o $(TARGET) -c portage-ng.pl

install: all
	$(SUDO) cp $(TARGET) /usr/local/bin
