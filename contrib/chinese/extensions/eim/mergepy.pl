#!/usr/bin/perl -w
# mergepy.pl --- Merge two eim-py table
# Last modify Time-stamp: <Ye Wenbin 2007-07-12 20:45:38>
# Version: v 0.0 2007/07/12 11:39:17
# Author: Ye Wenbin <wenbinye@163.com>

use strict;
use warnings;
use Pod::Usage;
use Getopt::Long qw(:config no_ignore_case auto_help);

my ($outfile);

GetOptions(
    'output=s' => \$outfile,
);

my ($prime_table, $added_table) = @ARGV;
my $max_index = 10000;
my %table;
my $outfh = \*STDOUT;

if ( $outfile ) {
    open(OUT, ">", $outfile) or die "Can't open file $outfile: $!";
    $outfh = \*OUT;
}

open(FH1, $prime_table) or die "Can't open file $prime_table: $!";
open(FH2, $added_table) or die "Can't open file $added_table: $!";

# read prime table
while ( <FH1> ) {
    print $outfh $_;
    last if /^\[Table\]$/;
}

while ( <FH1> ) {
    next if /^\s*$/;
    next if /^[^a-z]/;          # not right format
    chomp;
    my @r = split /\s+/;
    $table{$r[0]} = {
        map { $r[$_] => $_ } 1..$#r
    };
}

close FH1;

# add all items in $add_table to the end of the prime table
while ( <FH2> ) {
    last if /^\[Table\]$/;
}
while ( <FH2> ) {
    next if /^$/;
    next if /^[^a-z]/;          # not right format
    chomp;
    my @r = split /\s+/;
    my $item = $table{$r[0]};
    if ( defined $item ) {
        foreach ( 1..$#r ) {
            next if exists $item->{$r[$_]};
            $item->{$r[$_]} = $max_index + $_;
        }
    } else {
        $table{$r[0]} = {
            map { $r[$_] => $_ } 1..$#r
        };
    }
}
close FH2;
foreach ( sort keys %table ) {
    my $item = $table{$_};
    print $outfh join(' ', $_, sort {$item->{$a} <=> $item->{$b}} keys %$item), "\n";
}

if ( $outfile ) {
    close $outfh;
}

__END__

=head1 NAME

mergepy.pl - A utility to merge two eim-py table file

=head1 SYNOPSIS

mergepy.pl prime-table added-table [-o new_table]

Add all items in added-table to the prime-table. The duplicate items
will ignore and new items in added-table will append to prime-table
with the same order.

You may use this utility to update or enlarge your eim-py table file
without lose your own table.
