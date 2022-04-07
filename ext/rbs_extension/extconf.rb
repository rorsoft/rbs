require 'mkmf'
$INCFLAGS << " -I$(top_srcdir)" if $extmk
append_cflags ['-std=gnu99', '-Wold-style-definition']
create_makefile 'rbs_extension'
