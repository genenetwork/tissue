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

project = tissue
# FIXME: Do not hardcode the effective version.
guile_effective_version = 3.0

GUILD ?= guild

prefix ?= /usr/local
exec_prefix ?= $(prefix)
bindir ?= $(exec_prefix)/bin
libdir ?= $(exec_prefix)/lib
datarootdir ?= $(prefix)/share

top_level_module_dir = $(project)
sources = $(wildcard $(top_level_module_dir)/*.scm) $(wildcard $(top_level_module_dir)/web/*.scm)
objects = $(sources:.scm=.go)
scripts = $(wildcard bin/*)

scmdir = $(datarootdir)/guile/site/$(guile_effective_version)/$(top_level_module_dir)
godir = $(libdir)/guile/$(guile_effective_version)/site-ccache/$(top_level_module_dir)

.PHONY: all check install clean

all: $(objects)

%.go: %.scm
	GUILE_AUTO_COMPILE=0 $(GUILD) compile -L . -o $@ $<

check: ;

install:
	install -D $(scripts) --target-directory $(bindir)
	mkdir -p $(scmdir) $(godir)
	cp --parents -vr $(sources) $(scmdir)
	cp --parents -vr $(objects) $(godir)

clean:
	rm -f $(objects)
