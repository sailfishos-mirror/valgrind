# This file is part of Valgrind, a dynamic binary instrumentation
# framework.

# Copyright (C) 2026 Alexandra Hájková

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

# The GNU General Public License is contained in the file COPYING.

import gdb


class target_valgrind_command(gdb.Command):
    """target valgrind [args]

    Connect to a local extended-remote target running vgdb, this
    allows GDB to interact with valgrind.

    Any `args` are passed through to vgdb after the --vargs flag.
    """

    def __init__(self):
        gdb.Command.__init__(self, "target valgrind", gdb.COMMAND_NONE)

    def invoke(self, args, from_tty):
        vgdb = "target extended-remote | vgdb --multi"
        if len(args) > 0:
            vgdb += " --vargs " + args
        try:
            gdb.execute(vgdb, from_tty)
        except gdb.error as error:
            raise gdb.GdbError("Could not connect to Valgrind: {}".format(error)) from None


target_valgrind_command()
