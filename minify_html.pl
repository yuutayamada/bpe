#!/usr/bin/perl

use File::Spec;
use HTML::Packer;
use utf8;

my $input = "/tmp/emacs-bpe-tmp-file.html";
my $output = "/tmp/emacs.html";

my $packer = HTML::Packer->init();
my %opts = ("remove_comments" => 1,
            "remove_newlines" => 1);

my $input_file = File::Spec->rel2abs($ARGV[0]);
print "File name: \n";
print $input_file;

my $html = "";
open (FILE, $input_file) or die "$!";
while (my $line = <FILE>) {
  $html = $html.$line;
}
close (FILE);

my $minified_html = $packer->minify(\$html, $opts);

open(DATAFILE, ">", $ARGV[1]) or die("error :$!");
print DATAFILE $minified_html;
close(DATAFILE);
