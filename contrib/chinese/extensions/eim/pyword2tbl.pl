#! /usr/bin/perl -w          # -*- coding: utf-8 -*-
# pyword2tbl.pl --- Create a table for eim py input method
# Last modify Time-stamp: <Ye Wenbin 2007-07-12 20:48:30>
# Version: v 0.0 2006/07/17 06:29:58
# Author: Ye Wenbin <wenbinye@163.com>

use strict;
use warnings;
use Encode qw(encode decode from_to);
use Getopt::Long;
use Data::Dump qw(dump);
use Storable;

my ($type, $file, $coding, $help);
GetOptions("file=s" => \$file,
           "type=i" => \$type,
           "coding=s" => \$coding,
           "help|?" => \$help,               
          );
if ($help || !defined($type) || $type <0 || $type>3 || !defined($file) || $file eq "") {
    usage();
    exit;
}

$coding = $coding || 'gbk';

my %shenmu =map {$_ => 1 } qw(b p m f d t n l g k h j q x z c s zh ch sh r y w);
my %yumu = map {$_ => 1} qw (a o e i u v ai ei ui ao ou iu
		       ie ia ua ve er an en in un vn ang iong
		       eng ing ong uan uang ian iang iao ue
                  uai uo);
my $charpy = retrieve 'charpy.st';
if ($type == 1) {
    convert1($file);
} elsif ($type == 2) {
    convert2($file);
} else {
    convert3($file);
}

sub usage {
    print <<USAGE;
    $0 -file wordfile -type [1|2|3] -coding (utf8|gbk...)
      type 1: 
   把这样的文件转换成需要的格式：
   阿会喃
   韦昭
   伊籍
   异民族
   尹赏
   多音字会输出到标准错误中，等待进一步的处理。
      type 2:
   把这样的文件转换成需要的格式：
   贾逵 jia-kui
   贾余 jia-yu
   乐进 yue-jin
   郭石 guo-shi

      type 3:
   是把文件中的拼音合并，排序
USAGE
}

sub createCharpy {
    my $charfile = shift || "pychr.txt";
    my $coding = 'gbk';
    my %charpy;
    open(FH, $charfile) || die "Can't open file $charfile: $!";
    while (<FH>) {
        chomp;
        from_to($_, $coding, 'utf8');
        my @r = split /\s+/;
        foreach (1..$#r) {
            push @{$charpy{$r[$_]}}, $r[0];
        }
    }
    close FH;
    store \%charpy, 'charpy.st';
}

########################################
# 这个函数是把文件中的拼音合并，排序
########################################
sub convert3 {
    my $wordfile = shift;
    my %wordpy;
    open(FH, $wordfile) || die "Can't open file $wordfile: $!";
    while (<FH>) {
        chomp;
        my @res = split /\s+/;
        foreach (1..$#res) {
            $wordpy{$res[0]}{$res[$_]}++;
        }
    }
    close FH;
    foreach (sort keys %wordpy) {
        print STDOUT join(" ", $_,  keys %{$wordpy{$_}}), "\n";
    }
}

########################################
# 这个函数是把这样的文件转换成需要的格式：
# 贾逵 jia-kui
# 贾余 jia-yu
# 乐进 yue-jin
# 郭石 guo-shi
# 
########################################
sub convert2 {
    my $wordfile = shift;
    my %wordpy;
    open(FH, $wordfile) || die "Can't open file $wordfile: $!";
    while (<FH>) {
        chomp;
        my @res = split /\s+/;
        my @pys = split '-', $res[1];
        $wordpy{join("-", map  {(parseCharpy($_))[0] } @pys)}{$res[0]}++;
        $wordpy{$res[1]}{$res[0]}++;
    }
    close FH;
    foreach (sort keys %wordpy) {
        print STDOUT join(" ", $_,  keys %{$wordpy{$_}}), "\n";
    }
}

########################################
# 这个函数是把这样的文件转换成需要的格式：
# 阿会喃
# 韦昭
# 伊籍
# 异民族
# 尹赏
# 
# 多音字会输出到标准错误中，等待进一步的处理。
########################################
sub convert1 {
    my $wordfile = shift;
    open(FH, $wordfile) || die "Can't open file $wordfile: $!";
    my @dup;
    my $word;
    my $i = 0;
    my %wordpy;
    while ($word = <FH>) {
        chomp($word);
        from_to($word, $coding, 'utf8');
        my @char = ($word =~ m/(.{3})/g);
#         print join("-", @char), "\n";
#         return;
        my @res = ("");
        foreach (@char) {
            @res = mult_array(\@res, $charpy->{$_});
        }
        for (@res) {
            $_ = substr($_, 1);
        }
        if ($#res == 0) {
            my @pys = split '-', $res[0];
            $wordpy{join("-", map  {(parseCharpy($_))[0] } @pys)}{$word}++;
            $wordpy{$res[0]}{$word}++;
        } else {
            push @dup, [$word, @res];
        }
        last if $i++ > 100;
    }
    close FH;
    foreach (sort keys %wordpy) {
        print STDOUT join(" ", $_,  keys %{$wordpy{$_}}), "\n";
    }
    foreach (@dup) {
        print STDERR join(" ", @{$_}), "\n";
    }
}

sub mult_array {
    map {
        my $s = $_;
        map {
            "$s-$_";
        } @{$_[1]};
    } @{$_[0]};
}

sub parseCharpy {
    my $charpy = shift;
    my $sm;
    if (length($charpy) < 2) {
        return ($charpy, "");
    }
    else {
        $sm = substr($charpy, 0, 2);
        if (exists $shenmu{$sm}) {
            return ($sm, substr($charpy, 2));
        }
        else {
            return (substr($charpy, 0, 1), substr($charpy, 1));
        }
    }
}

__END__

=head1 NAME

pyword2tbl  ---  Create a table for eim py input method

=head1 DESCRIPTION

通常完成一个转换需要这样一个步骤：
1. 初步转换。这一步生成没有多音字的词组，有多音字的词组输出到标准错误中。
$ pyword2tbl.pl -f sanguo.txt -t 1 1> sanguo_r.txt 2> sanguo_e.txt

2. 修改多音字。把有多音字的词组中多余的拼音去掉。然后运行下面的命令。
$ pyword2tbl.pl -f sanguo_e.txt -t 2 >> sanguo_r.txt

3. 排序合并表格。
$ pyword2tbl.pl -f sanguo_r.txt -t 3 > sanguo.txt

如果输入的词库文件的编码不是 gbk，请在命令行中指定文件的编码。

=cut
