# tissue --- Text based issue tracker
# Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
#
# This file is part of tissue.
#
# tissue is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# tissue is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tissue.  If not, see <https://www.gnu.org/licenses/>.

prefix ?= /usr/local
exec_prefix ?= $(prefix)
bindir ?= $(exec_prefix)/bin

scripts = $(wildcard bin/*)

.PHONY: all check install

all: ;

check: ;

install: $(scripts)
	install -D $^ --target-directory $(bindir)
