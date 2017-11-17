#!/opt/local/bin/perl
#Perl script to extract data relating to a given set of criteria from a povray file for the whole SEM system
#Take folder location of data and criteria for extraction at command line.
#Loop over all 100 timepoint povray files.
#Create filename to read from at each time point.
#Read each line from file and use regular expressions to see if it contains *all* of the given criteri.
#If so, print this line into a new file created de novo for each timestep.

#Valid criteria to input at command line include:
#   - cellNN
#   - pair
#   - inter or intra
#   - element
#   - cortex
#   - volume
#   - triangle
#   - cortexpair
#   - colour, eg. Green combined with cortex shows only cortex pairs updated by DIT.

use strict;
use warnings;

defined($ARGV[0]) or die "No arguments - provide folder path in which to find data and at least one criterion for extraction";
defined($ARGV[1]) or die "Missing second argument - provide folder path and at least one criterion for extraction";

my $originalfolder = shift(@ARGV);
#Create the name of the directory into which data will be written
my $directoryname = join("_",$originalfolder."/extracted",@ARGV);
#Store remaining arguments in @expressions. These are the regular expressions to search for.
my @expressions = @ARGV;

#Make directory to contain extracted cell files
system "mkdir $directoryname";

#Input and output filenames and counter for while loop
my $file_name_in;
my $file_name_out;
my $i = 0;

#Loop over all 100 timepoints
while ( -e $originalfolder."/povray_data/snap_".sprintf("%02d",$i).".pov") {

  #Define filenames
  $file_name_in = $originalfolder."/povray_data/snap_".sprintf("%02d",$i).".pov";
  $file_name_out = $directoryname."/snap_".sprintf("%02d",$i).".pov";

  open(my $file_handle_out, '>', $file_name_out) or die "can't open output data file";
  open(my $file_handle_in, '<', $file_name_in) or die "can't open input data file";

  #For each line of the original povray file, check if it contains all regular
  #expressions specified at command line and if so print the line into the
  #new .pov file.
  #The following code block will test all lines in the original data file.
  #If all regular expressions are found in the line, $switch will remain equal
  #to 1 and the line will be printed into the new extracted data file.
  #If any regular expression is not found, $switch will be set equal to 0 and
  #the line will not be printed.
  while (my $line = <$file_handle_in>) {
    if (($line =~ /sphere/) or ($line =~ /cylinder/) or ($line =~ /smooth_/)) {
      if ($line =~ /boundary/) {
#        print $file_handle_out $line;
      }
      else {
        my $switch = 1;
        foreach my $expression (@expressions) {
          if ($line =~ /$expression/) {
            next; #Do nothing if regular expression is found and move on to next regular expression.
          }
          else {
            $switch=0; #Set $switch to 0 if an expression is not found.
          }
        }
        if ($switch==1) {
          print $file_handle_out $line;
        }
        else {
          next;
        }
      }
    }
    else {
      print $file_handle_out $line;
    }

#    if ($line =~ /boundary/) {
#      print $file_handle_out $line;
#    }
#    else {
#      my $switch = 1;
#      foreach my $expression (@expressions) {
#        if ($line =~ /$expression/) {
#          next; #Do nothing if regular expression is found and move on to next regular expression.
#        }
#        else {
#          $switch=0; #Set $switch to 0 if an expression is not found.
#        }
#      }
#      if ($switch==1) {
#        print $file_handle_out $line;
#      }
#      else {
#        next;
#      }
#    }
  }
  $i++; #Increment $i to move into next set of data (snapshot)
}

#system "cp scripts/visualise_povray_script.sh ".$directoryname
