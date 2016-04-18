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

use strict;
use warnings;

defined($ARGV[0]) or die "No arguments - provide folder name in which to find data and at lease one criterion for extraction";
defined($ARGV[1]) or die "Missing second argument - provide at least one criterion for extraction";

my $originalfolder = shift(@ARGV);
#Create the name of the directory into which data will be written
my $directoryname = join("_","../data/".$originalfolder."/extracted_",@ARGV);

#Store remaining arguments in @expressions. These are the regular expressions to search for.
my @expressions = @ARGV;

#Make directory to contain extracted cell files
system "mkdir $directoryname";

#Input and output filenames and counter for while loop
my $file_name_in;
my $file_name_out;
my $i = 0;

#Loop over all 100 timepoints
while ($i < 100) {

  #Define filenames
  if ($i < 10) {
    $file_name_in = "../data/".$originalfolder."/povray_data/snap_0".$i.".pov";
    $file_name_out = $directoryname."/snap_0".$i.".pov";
  }
  else {
    $file_name_in = "../data/".$originalfolder."/povray_data/snap_".$i.".pov";
    $file_name_out = $directoryname."/snap_".$i.".pov";
  }

  open(my $file_handle_out, '>', $file_name_out);
  open(my $file_handle_in, '<', $file_name_in) or die "can't open file";

  #Write setup lines to povray file
  print $file_handle_out " #version 3.5;\n";
  print $file_handle_out ' #include "colors.inc"', "\n";
  print $file_handle_out ' #include "textures.inc"', "\n";
  print $file_handle_out " background {White}\n";
  print $file_handle_out "";
  print $file_handle_out " camera {\n";
  print $file_handle_out "    location  <500, 0, 0>\n";
  print $file_handle_out "    angle 12\n";
  print $file_handle_out "    look_at<0,0,0>}\n";
  print $file_handle_out "";
  print $file_handle_out " light_source { < -60, 60, 0 > color White }\n";
  print $file_handle_out " light_source { < 60, -60, 0 > color White }\n";
  print $file_handle_out " light_source { < 0, 0, 60 > color White }\n";
  print $file_handle_out " light_source { < 0, 0, -60 > color White }\n";
  print $file_handle_out "";

  #For each line of the original povray file, check if it contains all regular
  #expressions specified at command line and if so print the line into the
  #new .pov file.
  #The following code block will test all lines in the original data file.
  #If all regular expressions are found in the line, $switch will remain equal
  #to 1 and the line will be printed into the new extracted data file.
  #If any regular expression is not found, $switch will be set equal to 0 and
  #the line will not be printed.
  while (my $line = <$file_handle_in>) {
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
  $i++; #Increment $i to move into next set of data (snapshot)
}

system "cp visualise_povray_script.sh ".$directoryname
