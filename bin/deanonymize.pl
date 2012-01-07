#!/usr/bin/perl -w

######################################################################
#
# Copyright (C) 2011, 2012 Michael Josef Beer <michael.josef.beer@googlemail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
######################################################################

=pod

=head1 NAME

deanonymize - Find names of movies for point vectors

=head1 SYNOPSIS

    ./deanonymize.pl POINT_FILE [-h]

=head1 DESCRIPTION

    Acts as a filter, thus you should use it like

    cat nameless-points | ./deanonymize.pl korrelation.csv > named-points

=cut

use strict;
use Data::Dumper;


sub usage {
    exec "pod2man $0 | man -l -";
}


sub readInData {
    my %movieBase = ();
    my $entry; 
    my ($first, $second) = (shift, shift);
    while($first && $second) {
        chomp $second;
        $first =~ /#\s*(.*).xml/ and do { 
            if($entry = $movieBase{$second}) {
                $movieBase{$second} = [$entry, $1];
            } else {
                $movieBase{$second} = [$1];
            }
        };
        ($first, $second) = (shift, shift);
    };
    return \%movieBase;
}


sub stripRow {
    my $score = shift;
    my @res;
    while(defined ($_ = shift)) {
        /(\d+)/ && push @res, "$1";
    };
    return \@res;
}


usage if ($_ = shift) and /-h/;
usage if (not $_);

open POINT_FILE, $_;
<POINT_FILE>;
my %mB = %{readInData((<POINT_FILE>))};
close POINT_FILE;

# for my $score (sort keys %mB) {
#     # # print "$score\n";
#     # print join ' ', ($score, ' ', @{$mB{$score}}, "\n");
# }

while($_ = <STDIN>) {
    /^ *#/ && do {
        print $_;
        next;
    };
    chomp $_;
    # print STDERR "'$_'\n";
    my @points = split ' *', $_;
    # print STDERR join "'", ("points ", @points, "\n");
    my @adapted = @{stripRow $_, @points};
    # print STDERR join "'", ("adapted ", @adapted, "\n");
    my $score = join " ", @adapted;
    my $movie = $mB{$score};
    $movie = ["NO MOVIE FOUND ???"] unless $movie;
    print STDOUT join ' ', ($score, @$movie, "\n");
}


=head1 AUTHOR

Michael J. Beer

=cut

