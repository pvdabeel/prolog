SUDO=sudo
TARGET=portage-ng

all:
	swipl -L8g -G8g -o $(TARGET) -O -q -f portage-ng.pl -g main --stand_alone=true -c portage-ng.pl

install: all
	$(SUDO) cp $(TARGET) /usr/local/bin
