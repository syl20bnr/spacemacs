package (>>>PERL_MODULE_NAME<<<);
#                                (>>>AUTHOR<<<)
#                                (>>>DATE<<<)


=head1 NAME

(>>>PERL_MODULE_NAME<<<) - TODO Perl extension for blah blah blah

=head1 SYNOPSIS

   use (>>>PERL_MODULE_NAME<<<) ':all';

   TODO

=head1 DESCRIPTION

TODO  Stub documentation for (>>>PERL_MODULE_NAME<<<),
created by perlnow.el using template.el.

It looks like the author of the extension was negligent
enough to leave the stub unedited.

=head2 EXPORT

None by default.

=cut

use (>>>MINIMUM_PERL_VERSION<<<);
use strict;
use warnings;
## use Carp;
## use Data::Dumper;

require Exporter;

our @ISA = qw(Exporter);
our %EXPORT_TAGS = ( 'all' => [
  # TODO Add names of items to export here.
  qw(


    ) ] );
# The above allows declaration	use (>>>PERL_MODULE_NAME<<<) ':all';
# Moving things directly into @EXPORT or (better) @EXPORT_OK saves some memory.
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw(  ); # items to export into callers namespace by default.
                      # (don't use this without a very good reason.)
our $VERSION = '0.01';

# Preloaded methods go here.
(>>>POINT<<<)


1;

=head1 SEE ALSO

TODO Mention other useful documentation:

  o  related modules:  L<Module::Name>
  o  operating system documentation (such as man pages in UNIX)
  o  any relevant external documentation such as RFCs or standards
  o  discussion forum set up for your module (if you have it)
  o  web site set up for your module (if you have it)

=head1 AUTHOR

(>>>USER_NAME<<<), E<lt>(>>>EMAIL_DOT_EMACS<<<)E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)

=head1 BUGS

None reported... yet.

=cut
