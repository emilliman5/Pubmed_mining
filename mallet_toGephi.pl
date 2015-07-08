#!/usr/bin/perl

use strict;

open (IN,"<$ARGV[0]") || die "Could not open $ARGV[0]\n";
open (OUT, ">>$ARGV[1]");

while (<IN>){
	chomp;
	my @line=split("\t", $_);
	for(my $i=2; $i<=$#line; $i++){
		my $t=$i-2;
		print OUT $line[1]."\t".$t."\t".$line[$i]."\n";
	}
}