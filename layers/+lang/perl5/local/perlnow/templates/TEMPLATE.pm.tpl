package (>>>module-name<<<);
# (>>>AUTHOR<<<)          (>>>DATE<<<)

use 5.006;
use strict; 
use warnings;
use Carp;
use Data::Dumper;

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use (>>>module-name<<<) ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
  (>>>9<<<)
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw( (>>>8<<<) );

our $VERSION = '0.01';

# Preloaded methods go here.
(>>>POINT<<<)

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

(>>>module-name<<<) - Perl extension for blah blah blah

=head1 SYNOPSIS

   use (>>>module-name<<<);
   (>>>7<<<)blah blah blah

=head1 DESCRIPTION

(>>>6<<<)Stub documentation for (>>>module-name<<<), 
created by template.el.

It looks like the author of the extension was negligent
enough to leave the stub unedited.

Blah blah blah.

=head2 EXPORT

None by default.

=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut

>>>TEMPLATE-DEFINITION-SECTION<<<
("module-name" "Module Package Name: ")
