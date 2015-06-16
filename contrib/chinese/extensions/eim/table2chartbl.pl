#!/usr/bin/perl -w
# table2chartbl.pl --- 
# Author: Ye Wenbin <wenbinye@gmail.com>
# Created: 27 Apr 2008
# Version: 0.01

use warnings;
use strict;
use Getopt::Long;
use utf8;
use Pod::Usage;

my ($filter, $im, $output);
binmode STDOUT, "utf8"; 

GetOptions(
    "filter=s" => \$filter,
    "im=s" => \$im,
    "output=s" => \$output,
);
if ( !defined $im ) {
    pod2usage("Input method name needed!\n");
}

unless ( @ARGV ) {
    pod2usage("Table files are not given!\n");
}

my %table;
foreach my $file ( @ARGV ) {
    open(my $fh, "<:utf8", $file) or die "Can't open file $file: $!";
    while ( <$fh> ) {
        last if /^\[Table\]/;
    }
    while ( <$fh> ) {
        my @r = split /\s+/;
        foreach my $c ( @r[1..$#r] ) {
            if ( length($c) == 1 ) {
                if ( exists $table{$c} ) {
                    if ( length($r[0]) > length($table{$c}) ) {
                        $table{$c} = $r[0];
                    }
                }
                else {
                    $table{$c} = $r[0];
                }
            }
        }
    }
}

my %rtable;
foreach ( keys %table ) {
    push @{$rtable{$table{$_}}}, $_;
}

$output ||= "eim-".$im."-chars.el";
open(my $fh, ">:utf8", $output) or die "Can't create file $output: $!";
print "Save table to $output...\n";

print {$fh} <<'HEADER';
;;; -*- coding: utf-8 -*-
;;;_. 字库
(eim-make-char-table '(
HEADER

foreach ( sort keys %rtable ) {
    s/\\/\\\\/;
    s/"/\\"/;
    if ( $filter && /$filter/ ) {
        next;
    }
    print {$fh} "(", join(' ', map { qq("$_") } $_, @{$rtable{$_}}), ")\n";
}

print {$fh} <<"FOOTER";
) eim-$im-char-table)
FOOTER

print "Done!\n"

__END__

=head1 NAME

table2chartbl.pl - 从码表中提取汉字到编码的转换表

=head1 SYNOPSIS

table2chartbl.pl [-i im -f filter -o output] tables

      -i --im         输入法名称
      -f --filter     对码表过滤
      -o --output     输出文件

=head1 DESCRIPTION

本程序用于从 eim 的输入法码表中提取汉字的转换表。

=head1 AUTHOR

Ye Wenbin, E<lt>wenbinye@gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008 by Ye Wenbin

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
