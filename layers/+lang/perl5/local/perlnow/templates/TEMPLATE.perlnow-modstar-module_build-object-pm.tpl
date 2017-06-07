# An OOP template for module-starter with Module::Build
package (>>>PERL_MODULE_NAME<<<);
use base qw( Class::Base );

use warnings;
use strict;

=head1 NAME

(>>>PERL_MODULE_NAME<<<) - The great new (>>>PERL_MODULE_NAME<<<)! TODO revise this

=head1 VERSION

Version 0.01

=cut

# TODO revise these before shipping
our $VERSION = '0.01';
my $DEBUG = 1;

=head1 SYNOPSIS

   use (>>>PERL_MODULE_NAME<<<);
   my $obj = (>>>PERL_MODULE_NAME<<<)->new({ ...  });

   # TODO expand on this

=head1 DESCRIPTION

(>>>PERL_MODULE_NAME<<<) is a module that ...

TODO expand this stub documentation, which was created by perlnow.el.

=head1 METHODS

=over

=cut

use (>>>MINIMUM_PERL_VERSION<<<);
use Carp;
use Data::Dumper;
use Hash::Util qw( lock_keys unlock_keys );

# needed for accessor generation
our $AUTOLOAD;
my %ATTRIBUTES = ();

=item new

Creates a new (>>>PERL_MODULE_NAME<<<) object.

Takes a hashref as an argument, with named fields identical
to the names of the object attributes. These attributes are:

=over

=item <fill-in attributes here... most likely, sort in order of utility>

=back

Takes an optional hashref as an argument, with named fields
identical to the names of the object attributes.

=cut

# Note: "new" is inherited from Class::Base and
# calls the following "init" routine automatically.

=item init

Method that initializes object attributes and then locks them
down to prevent accidental creation of new ones.

Any class that inherits from this one should have an B<init> of
it's own that calls this B<init>.  Otherwise, it's an internally
used routine that is not of much interest to client coders.

=cut

sub init {
  my $self = shift;
  my $args = shift;
  # $self->SUPER::init( $args );  # uncomment if this is a child class
  unlock_keys( %{ $self } );

  # enter object attributes here, including arguments that become attributes
  my @attributes = qw(
                      );
  # Note: accessors are generated automatically, but documentation
  # for them is not.

  # Alternately: uncomment this line, to automatically handle all given args
  # push @attributes, keys %{ $args };

  foreach my $field (@attributes) {
    $ATTRIBUTES{ $field } = 1;
    $self->{ $field } = $args->{ $field };
  }

  $DB::single = 1 if ($DEBUG); # set a debugger breakpoint here

  lock_keys( %{ $self } );
  return $self;
}

### Fill in additional methods here
### hint: perlnow-insert-method


=back

=head2 basic setters and getters

The naming convention in use here is that setters begin with
"set_", but getters have *no* prefix: the most commonly used case
deserves the simplest syntax (and mutators are deprecated).

These accessors exist for all of the object attributes (documented
above) irrespective of whether they're expected to be externally useful.

=head2  automatic generation of accessors

=over

=item AUTOLOAD

=cut

sub AUTOLOAD {
  return if $AUTOLOAD =~ /DESTROY$/;  # skip calls to DESTROY ()

  my ($name) = $AUTOLOAD =~ /([^:]+)$/; # extract method name
  (my $field = $name) =~ s/^set_//;

  # check that this is a valid accessor call
  croak("Unknown method '$AUTOLOAD' called")
    unless defined( $ATTRIBUTES{ $field } );

  { no strict 'refs';

    # create the setter and getter and install them in the symbol table

    if ( $name =~ /^set_/ ) {

      *$name = sub {
        my $self = shift;
        $self->{ $field } = shift;
        return $self->{ $field };
      };

      goto &$name;              # jump to the new method.
    } elsif ( $name =~ /^get_/ ) {
      carp("Apparent attempt at using a getter with unneeded 'get_' prefix.");
    }

    *$name = sub {
      my $self = shift;
      return $self->{ $field };
    };

    goto &$name;                # jump to the new method.
  }
}

=back

=head1 AUTHOR

(>>>USER_NAME<<<), E<lt>(>>>EMAIL_DOT_EMACS<<<)E<gt>,
(>>>DATE<<<)

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)

=head1 BUGS

Please report any bugs or feature requests to C<bug-emacs-run at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Emacs-Run>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Emacs::Run

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Emacs-Run>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Emacs-Run>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Emacs-Run>

=item * Search CPAN

L<http://search.cpan.org/dist/Emacs-Run/>

=back

=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

1; # End of Emacs::Run
