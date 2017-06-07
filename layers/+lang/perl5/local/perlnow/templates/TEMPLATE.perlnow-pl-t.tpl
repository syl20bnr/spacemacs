# Perl test file, which can be run like so:
#   `perl (>>>FILE<<<)'
#        (>>>AUTHOR<<<)     (>>>VC_DATE<<<)

use warnings;
use strict;
$|=1;
my $DEBUG = 1;
use Data::Dumper;

use Test::More;
BEGIN { plan tests => 1 };  # TODO # change to 'tests => last_test_to_print';

ok(1, "Traditional: If we made it this far, we're ok.");

# TODO Enter the expected output from the script
my $expected=<<"EXPECTED";
(>>>POINT<<<)
EXPECTED

# TODO any arguments to add after the script name?
is( `(>>>PERL_SCRIPT_NAME<<<)` , $expected, "Test name: (>>>2<<<)")



