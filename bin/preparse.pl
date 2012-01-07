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

preparse - converts Stefans ominous mostly-csv format into something
slightly more pleasing  ;)

=head1 SYNOPSIS

    ./preparse.pl [-h]

=head1 DESCRIPTION

Converts Stefans not quite csv format into 'standard form', that is one record
per line. The format is 
 
    SCORE  MOVIE_FILE_NAME_WITHOUT_XML_ENDING

such that

    # whats_up_doc.xml
    1 2 3

becomes 

    1 2 3 whats_up_doc

 If two movies with the same score are found, their antries are merged
such that their score is printed followed by both names, i.e. 

    # whats_up_doc.xml
    1 2 3
    # die_tollkuehnen_maenner_in_ihren_fliegendeb_kisten.xml
    1 2 3

become

    1 2 3 whats_up_doc die_tollkuehnen_maenner_in_ihren_fliegenden_kisten

Acts as a pure filter by strictly reading from STDIN and writing to STDOUT only.

-h prints this help page.

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


usage if ($_ = shift) and /-h/;

<>;
my %mB = %{readInData((<>))};

for my $score (sort keys %mB) {
    print "$score\n";
    # print join ' ', ($score, ' ', @{$mB{$score}}, "\n");
}


=head1 AUTHOR

Michael J. Beer

=cut

