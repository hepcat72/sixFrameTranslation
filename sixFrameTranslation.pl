#!/usr/bin/perl -w

#sixFrameTranslation.pl
#Generated using perl_script_template.pl 1.34
#Robert W. Leach
#rwleach@ccr.buffalo.edu
#Created on 8/6/2008
#Center for Computational Research
#Copyright 2008

#These variables (in main) are used by printVersion()
my $template_version_number = '1.34';
my $software_version_number = '1.4';

##
## Start Main
##

use strict;
use Getopt::Long;

#Declare & initialize variables.  Provide default values here.
my($outfile_suffix); #Not defined so a user can overwrite the input file
my @input_files         = ();
my $current_output_file = '';
my $help                = 0;
my $version             = 0;
my $force               = 0;
my $genetic_code        = 11;
my $stop_char           = '*';
my $do_not_append       = 0;
my $valid_seqs_only     = 0;
my $allow_partials      = 0;
my $min_full_length     = 0;
my $min_part_length     = 0;
my $ambig_char          = 'X';
my $dna_suffix          = '';

#These variables (in main) are used by the following subroutines:
#verbose, error, warning, debug, printVersion, getCommand and usage
my $preserve_args = [@ARGV];  #Preserve the agruments for getCommand
my $verbose       = 0;
my $quiet         = 0;
my $DEBUG         = 0;
my $nowarn        = 0;

my $GetOptHash =
  {'i|fasta-file=s'     => sub {push(@input_files,   #REQUIRED unless <> is
				     sglob($_[1]))}, #         supplied
   '<>'                 => sub {push(@input_files,   #REQUIRED unless -i is
				     sglob($_[0]))}, #         supplied
   'g|genetic-code=s'   => \$genetic_code,           #OPTIONAL [11]
   's|stop-character=s' => \$stop_char,              #OPTIONAL [*]
   'a|ambig-character=s'=> \$ambig_char,             #OPTIONAL [X]
   'd|do-not-append-to-id!' => \$do_not_append,      #OPTIONAL [Off]
   'v|valid-seqs-only!' => \$valid_seqs_only,        #OPTIONAL [Off]
   'p|allow-partials!'  => \$allow_partials,         #OPTIONAL [Off]
   'f|min-full-length=s'=> \$min_full_length,        #OPTIONAL [Off]
   'r|min-part-length=s'=> \$min_part_length,        #OPTIONAL [Off]
   'o|outfile-suffix=s' => \$outfile_suffix,         #OPTIONAL [undef]
   'dna-suffix=s'       => \$dna_suffix,             #OPTIONAL [none]
   'force!'             => \$force,                  #OPTIONAL [Off]
   'verbose!'           => \$verbose,                #OPTIONAL [Off]
   'quiet!'             => \$quiet,                  #OPTIONAL [Off]
   'nowarn!'            => \$nowarn,                 #OPTIONAL [Off]
   'h|help!'            => \$help,                   #OPTIONAL [Off]
   'debug!'             => \$DEBUG,                  #OPTIONAL [Off]
   'version!'           => \$version,                #OPTIONAL [Off]
  };

if($dna_suffix ne '' && !$valid_seqs_only)
  {
    warning("You cannot supply --dna-suffix without the -v option to output ",
	    "only valid sequences.  This is because the output DNA sequences ",
	    "would simply be the same as the input sequences.  Continuing ",
	    "without outputting DNA files.");
    $dna_suffix = '';
  }

if($stop_char !~ /^.$/)
  {
    error("The stop character must be a single character.  ",
	  "You supplied: [$stop_char]");
    exit(7);
  }

if($ambig_char !~ /^.$/)
  {
    error("The ambiguous character must be a single character.  ",
	  "You supplied: [$ambig_char]");
    exit(7);
  }

if($min_full_length > 0 && !$valid_seqs_only)
  {
    error("--min-full-length may only be set if --valid-seqs-only is ",
	  "supplied.");
    exit(8);
  }

if($min_part_length > $min_full_length)
  {
    error("--min-part-length ($min_part_length) must be smaller than ",
	  "--min-full-length ($min_full_length).");
    exit(9);
  }

if($min_part_length > 0 && !$allow_partials)
  {
    warning("--min-part-length is only used when --allow-partials is ",
	    "supplied.");
  }

#If there are no arguments and no files directed or piped in
if(scalar(@ARGV) == 0 && isStandardInputFromTerminal())
  {
    usage();
    exit(0);
  }

#Get the input options
GetOptions(%$GetOptHash);

#Print the debug mode (it checks the value of the DEBUG global variable)
debug("Debug mode on.");

#If the user has asked for help, call the help subroutine
if($help)
  {
    help();
    exit(0);
  }

#If the user has asked for the software version, print it
if($version)
  {
    printVersion();
    exit(0);
  }

#Check validity of verbosity options
if($verbose && $quiet)
  {
    $quiet = 0;
    error("You cannot supply verbose and quiet flags at the same time.");
    exit(1);
  }

#Put standard input into the input_files array if standard input has been redirected in
if(!isStandardInputFromTerminal())
  {
    push(@input_files,'-');

    #Warn the user about the naming of the outfile when using STDIN
    if(defined($outfile_suffix))
      {warning("Input on STDIN detected along with an outfile suffix.  Your ",
	       "output file will be named STDIN$outfile_suffix")}
  }

#Make sure there is input
if(scalar(@input_files) == 0)
  {
    error("No input files detected.");
    usage(1);
    exit(2);
  }

#Check to make sure previously generated output files won't be over-written
#Note, this does not account for output redirected on the command line
if(!$force && defined($outfile_suffix))
  {
    my $existing_outfiles = [];
    foreach my $output_file (map {($_ eq '-' ? 'STDIN' : $_) . $outfile_suffix}
			     @input_files)
      {push(@$existing_outfiles,$output_file) if(-e $output_file)}

    if(scalar(@$existing_outfiles))
      {
	error("The output files: [@$existing_outfiles] already exist.  ",
	      "Use -f to force overwrite.  E.g.\n\t",
	      getCommand(1),' --force');
	exit(3);
      }
  }

if(isStandardOutputToTerminal() && !defined($outfile_suffix))
  {verbose("NOTE: VerboseOverMe functionality has been altered to yield ",
	   "clean STDOUT output.")}

verbose("Run conditions: ",getCommand(1),"\n");

#If output is going to STDOUT instead of output files with different extensions
if(!defined($outfile_suffix))
  {verbose("[STDOUT] Opened for all output.")}

my $current_dna_file = '';

#For each input file
foreach my $input_file (@input_files)
  {
    #If an output file name suffix has been defined
    if(defined($outfile_suffix))
      {
	##
	## Open and select the next output file
	##

	#Set the current output file name
	$current_output_file = ($input_file eq '-' ? 'STDIN' : $input_file)
	  . $outfile_suffix;

	#Open the output file
	if(!open(OUTPUT,">$current_output_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output file: [$current_output_file]\n$!");
	    next;
	  }
	else
	  {verboseOverMe("[$current_output_file] Opened output file.")}

	#Select the output file handle
	select(OUTPUT);
      }

    if($dna_suffix ne '')
      {
	##
	## Open and select the next DNA output file
	##

	#Set the current output file name
	$current_dna_file = ($input_file eq '-' ? 'STDIN' : $input_file)
	  . $dna_suffix;

	#Open the output file
	if(!open(DNA,">$current_dna_file"))
	  {
	    #Report an error and iterate if there was an error
	    error("Unable to open output DNA file: [$current_dna_file]\n$!");
	    next;
	  }
	else
	  {verboseOverMe("[$current_dna_file] Opened output DNA file.")}
      }

    #Open the input file
    if(!open(INPUT,$input_file))
      {
	#Report an error and iterate if there was an error
	error("Unable to open input file: [$input_file]\n$!");
	next;
      }
    else
      {verboseOverMe("[",
		     ($input_file eq '-' ? 'STDIN' : $input_file),
		     "] Opened input file.")}

    my $line_num     = 0;
    my $verbose_freq = 1000;


    #For each fasta record
    my($rec,$defline,$sequence);
    while($rec = getNextFastaRec(*INPUT))
      {
	$line_num++;
	verboseOverMe("[",
		      ($input_file eq '-' ? 'STDIN' : $input_file),
		      "] Reading record: [$line_num].") unless($line_num %
							     $verbose_freq);

	($defline,$sequence) = @$rec;

	if(!defined($sequence) || $sequence eq '')
	  {
	    error("No sequence was found for defline: [$defline] in file: ",
		  "[$input_file] .");
	    next;
	  }
	elsif(length($sequence) < 3)
	  {
	    warning("Sequence: [$defline] in file: [$input_file] was too ",
		    "short to translate.");
	    next;
	  }

	my $is_dna = isDNA($sequence);
	if($is_dna < .9)
	  {error("Sequence does not appear to be DNA: [$defline] in file: ",
		 "[$input_file].")}

	my $tmpseq = $sequence;
	my $orf_id = 1;
	my(@seqs,@gene_seqs,@gene_coords);
	my $first_is_partial = 0;
	my $last_is_partial  = 0;
	my @seq_holder = ();
	foreach my $frame (1..3)
	  {
	    $first_is_partial = 0;
	    $last_is_partial  = 0;

	    if($frame == 1)
	      {
		@seq_holder = translate($tmpseq,
					$genetic_code,
					$valid_seqs_only,
					$allow_partials,
					undef,
					$stop_char,
					$ambig_char,
					$min_full_length,
					$min_part_length);
		#To make translate backward compatible, I added this kluge
		#based on whether we're getting a single sequence back or a
		#bunch of ORF sequences (both protein and DNA)
		if($valid_seqs_only)
		  {
		    @seqs        = @{$seq_holder[0]};
		    @gene_seqs   = @{$seq_holder[1]};

		    #No need to adjust coordinates for frame 1
		    @gene_coords = @{$seq_holder[2]};

		    $first_is_partial = $seq_holder[3];
		    $last_is_partial  = $seq_holder[4];
		  }
		else
		  {push(@seqs,@seq_holder)}
	      }
	    else
	      {
		next if(length($tmpseq) < 3);
		$tmpseq  =~ s/.//;
		@seq_holder = translate($tmpseq,
					$genetic_code,
					$valid_seqs_only,
					$allow_partials,
					undef,
					$stop_char,
					$ambig_char,
					$min_full_length,
					$min_part_length);
		#To make translate backward compatible, I added this kluge
		#based on whether we're getting a single sequence back or a
		#bunch of ORF sequences (both protein and DNA)
		if($valid_seqs_only)
		  {
		    @seqs        = @{$seq_holder[0]};
		    @gene_seqs   = @{$seq_holder[1]};

		    #Adjust the coordinates for the frame
		    @gene_coords =
		      map {{'START' => ($_->{START} + ($frame - 1)),
			      'STOP'  => ($_->{STOP} + ($frame - 1))}}
			@{$seq_holder[2]};

		    debug("FIRST: SEQ: $gene_seqs[0] START: ",
			  $gene_coords[0]->{START},
			  " STOP: $gene_coords[0]->{STOP}")
		      if(scalar(@gene_seqs));

		    $first_is_partial = $seq_holder[3];
		    $last_is_partial  = $seq_holder[4];
		  }
		else
		  {push(@seqs,@seq_holder)}
	      }

	    if(scalar(@seqs) == 0 || (scalar(@seqs) == 1 && $seqs[0] eq ''))
	      {
		@seqs = ('') if(!$valid_seqs_only);
		@gene_seqs = ('') if(!$valid_seqs_only);
		warning("Frame $frame of sequence: [$defline] in file: ",
			"[$input_file] produced no proteins.");
	      }

	    my $seqnum = 0;
#	    foreach my $seq (@seqs)
	    if(scalar(@seqs) != scalar(@gene_seqs))
	      {
		error("The number of protein sequences is not the same as ",
		      "the number of gene sequences.  This should not have ",
		      "happened.  There appears to be a bug in the script.  ",
		      "See $defline in file $input_file.");
	      }
	    if(scalar(@gene_coords) != scalar(@gene_seqs))
	      {
		error("The number of gene sequences: [",scalar(@gene_seqs),
		      "] is not the same as the ",
		      "number of gene sequence coordinates: [",
		      scalar(@gene_coords),"].  This should not ",
		      "have happened.  There appears to be a bug in the ",
		      "script.  See $defline in file $input_file.");
	      }
	    foreach my $seq_index (0..(scalar(@seqs) - 1))
	      {
		my $full_partial = (($seq_index == 0 && $first_is_partial) ||
				    ($seq_index == (scalar(@seqs) - 1) &&
				     $last_is_partial) ?
				    'PARTIAL' : 'COMPLETE');
		my $seq        = $seqs[$seq_index];
		my $gene_seq   = $gene_seqs[$seq_index];
		my $gene_start = $gene_coords[$seq_index]->{START};
		my $gene_stop  = $gene_coords[$seq_index]->{STOP};

		++$seqnum;
		my $new_defline = $defline;
		unless($do_not_append)
		  {
		    if($valid_seqs_only)
		      {
			$new_defline =~ s/(?= |\Z)/.$orf_id/;
			$orf_id++;
		      }
		    else
		      {$new_defline =~ s/(?= |\Z)/.+$frame/}
		  }

		debug("NUM:$seqnum $new_defline $full_partial Coords: ",
		      "$gene_start..$gene_stop Frame: +$frame\n$seq\n");
		print("$new_defline $full_partial Coords: $gene_start..",
		      "$gene_stop Frame: +$frame\n$seq\n");
		if($dna_suffix ne '' && $valid_seqs_only)
		  {
		    debug("new_defline not defined in frame +$frame in ",
			  "$input_file.") if(!defined($new_defline));
		    debug("Frame not defined in $new_defline +$frame in ",
			  "$input_file.") if(!defined($frame));
		    debug("gene_seq not defined in $new_defline +$frame in ",
			  "$input_file.") if(!defined($gene_seq));
		    print DNA ("$new_defline $full_partial Coords: ",
			       "$gene_start..$gene_stop Frame: +$frame\n",
			       "$gene_seq\n");
		  }
	      }
	  }

	my $tmprseq = reverseComplement($sequence);
	my $seqlen  = length($tmprseq);
	debug("Translation of sequence: [$sequence] failed.  Type of ",
	      "returned variable: [",ref($tmprseq),"].")
	  if(!defined($tmprseq) || $tmprseq eq '');
	my(@rseqs,@gene_rseqs,@gene_rcoords);
	foreach my $frame (1..3)
	  {
	    $first_is_partial = 0;
	    $last_is_partial  = 0;

	    if($frame == 1)
	      {
		@seq_holder = translate($tmprseq,
					$genetic_code,
					$valid_seqs_only,
					$allow_partials,
					undef,
					$stop_char,
					$ambig_char,
					$min_full_length,
					$min_part_length);
		#To make translate backward compatible, I added this kluge
		#based on whether we're getting a single sequence back or a
		#bunch of ORF sequences (both protein and DNA)
		if($valid_seqs_only)
		  {
		    @rseqs      = @{$seq_holder[0]};
		    @gene_rseqs = @{$seq_holder[1]};

		    #Adjust the coordinates for reverse frame
		    @gene_rcoords =
		      map {{'START' => ($seqlen - $_->{START} + 1),
			      'STOP'  => ($seqlen - $_->{STOP} + 1)}}
			@{$seq_holder[2]};

		    $first_is_partial = $seq_holder[3];
		    $last_is_partial  = $seq_holder[4];
		  }
		else
		  {push(@rseqs,@seq_holder)}
	      }
	    else
	      {
		next if(length($tmprseq) < 3);
		$tmprseq =~ s/.//;
		@seq_holder = translate($tmprseq,
					$genetic_code,
					$valid_seqs_only,
					$allow_partials,
					undef,
					$stop_char,
					$ambig_char,
					$min_full_length,
					$min_part_length);
		#To make translate backward compatible, I added this kluge
		#based on whether we're getting a single sequence back or a
		#bunch of ORF sequences (both protein and DNA)
		if($valid_seqs_only)
		  {
		    @rseqs      = @{$seq_holder[0]};
		    @gene_rseqs = @{$seq_holder[1]};

		    #Adjust the coordinates for reverse frame & shift
		    @gene_rcoords =
		      map {{'START' => ($seqlen - ($_->{START} + ($frame - 1))
					+ 1),
			      'STOP'  => ($seqlen - ($_->{STOP} + ($frame - 1))
					  + 1)}}
			@{$seq_holder[2]};

		    $first_is_partial = $seq_holder[3];
		    $last_is_partial  = $seq_holder[4];
		  }
		else
		  {push(@rseqs,@seq_holder)}
	      }

	    if(scalar(@rseqs) == 0 || (scalar(@rseqs) == 1 && $rseqs[0] eq ''))
	      {
		warning("The reverse complement of frame -$frame of ",
			"sequence: [$defline] in file: [$input_file] ",
			"produced no proteins.");
		@rseqs = ('') if(!$valid_seqs_only);
		@gene_rseqs = ('') if(!$valid_seqs_only);
	      }

	    my $seqnum = 0;
#	    foreach my $rseq (@rseqs)
	    if(scalar(@rseqs) != scalar(@gene_rseqs))
	      {
		error("The number of protein sequences is not the same as ",
		      "the number of gene sequences.  This should not have ",
		      "happened.  The appears to be a bug in the script.  ",
		      "See $defline in file $input_file.");
	      }
	    if(scalar(@gene_rcoords) != scalar(@gene_rseqs))
	      {
		error("The number of gene sequences: [",scalar(@gene_rseqs),
		      "] is not the same as the ",
		      "number of gene sequence coordinates: [",
		      scalar(@gene_rcoords),"].  This should not ",
		      "have happened.  There appears to be a bug in the ",
		      "script.  See $defline in file $input_file.");
	      }
	    foreach my $seq_index (0..(scalar(@rseqs) - 1))
	      {
		my $full_partial = (($seq_index == 0 && $first_is_partial) ||
				    ($seq_index == (scalar(@seqs) - 1) &&
				     $last_is_partial) ?
				    'PARTIAL' : 'COMPLETE');
		my $rseq        = $rseqs[$seq_index];
		my $gene_rseq   = $gene_rseqs[$seq_index];
		my $gene_rstart = $gene_rcoords[$seq_index]->{START};
		my $gene_rstop  = $gene_rcoords[$seq_index]->{STOP};

		++$seqnum;
		my $new_defline = $defline;
		unless($do_not_append)
		  {
		    if($valid_seqs_only)
		      {
			$new_defline =~ s/(?= |\Z)/.$orf_id/;
			$orf_id++;
		      }
		    else
		      {$new_defline =~ s/(?= |\Z)/.-$frame/}
		  }

		debug("NUM:$seqnum $new_defline $full_partial Coords: ",
		      "$gene_rstart..$gene_rstop Frame: -$frame\n$rseq\n");
		print("$new_defline $full_partial Coords: $gene_rstart..",
		      "$gene_rstop Frame: -$frame\n$rseq\n");
		if($dna_suffix ne '' && $valid_seqs_only)
		  {
		    debug("new_defline not defined in frame -$frame in ",
			  "$input_file.") if(!defined($new_defline));
		    debug("Frame not defined in $new_defline -$frame in ",
			  "$input_file.") if(!defined($frame));
		    debug("gene_rseq not defined in $new_defline -$frame in ",
			  "$input_file.") if(!defined($gene_rseq));
		    print DNA ("$new_defline $full_partial Coords: ",
			       "$gene_rstart..$gene_rstop Frame: -$frame\n",
			       "$gene_rseq\n");
		  }
	      }
	  }
      }

    close(INPUT);

    verbose("[",
	    ($input_file eq '-' ? 'STDIN' : $input_file),
	    '] Input file done.  Time taken: [',
	    scalar(markTime()),
	    " Seconds].");

    if($dna_suffix ne '')
      {
	close(DNA);
	verbose("[$current_dna_file] Output file done.");
      }

    #If an output file name suffix is set
    if(defined($outfile_suffix))
      {
	#Select standard out
	select(STDOUT);
	#Close the output file handle
	close(OUTPUT);

	verbose("[$current_output_file] Output file done.");
      }
  }

#Report the number of errors, warnings, and debugs
verbose("Done.  EXIT STATUS: [",
	"ERRORS: ",
	($main::error_number ? $main::error_number : 0),
	" WARNINGS: ",
	($main::warning_number ? $main::warning_number : 0),
	($DEBUG ?
	 " DEBUGS: " . ($main::debug_number ? $main::debug_number : 0) : ''),
        " TIME: ",scalar(markTime(0)),"s]");
if($main::error_number || $main::warning_number)
  {verbose("Scroll up to inspect errors and warnings.")}

##
## End Main
##






























##
## Subroutines
##

#Copied from BioRob.pm 8/6/2008 -Rob
#Returns 0 if sequence contains non-DNA characters.  Note, it allows ambiguous
#DNA characters.  Case insensitive.  Note, returns true if sequence is empty.
#If unambiguous is false, returns the fraction of unambiguous characters
#contained.  Returns -1 if ambiguous characters are allowed, but no unambiguous
#characters were found.
sub isDNA
  {
#    my $self          = shift(@_);
    my $sequence      = $_[0];
    my $no_ambiguous  = $_[1];   #if true, responds false if BDHVRYKMSWN presnt
    my $no_whitespace = $_[2];   #if true, responds false if white space found
    my $allow_mask    = $_[3];   #Allows DNA to have X's
    my $allow_align   = $_[4];   #Allows dashes (-)
    my $allow_ignore  = $_[5];   #Allows DNA to have dots (.)

    if(wantarray)
      {
	my @nonATGCwhite;
	@nonATGCwhite = ($sequence =~ /([^ATGC])/ig);
	@nonATGCwhite = grep {/\s/} @nonATGCwhite if(!$no_whitespace);
	@nonATGCwhite = grep {/[^BDHVRYKMSWN]/i} @nonATGCwhite
	  if(!$no_ambiguous);
	@nonATGCwhite = grep {/[^X]/i} @nonATGCwhite if($allow_mask);
	@nonATGCwhite = grep {/[^\-]/i} @nonATGCwhite if($allow_align);
	@nonATGCwhite = grep {/[^\.]/i} @nonATGCwhite if($allow_ignore);
	return(@nonATGCwhite);
      }

    if($sequence =~ /[^ATGCBDHVRYKMSWNX\-\.\s]/i)
      {return(0)}
    if($no_whitespace && $sequence =~ /\s/)  #not ambiguous
      {return(0)}
    if(!$allow_ignore && $sequence =~ /\./)  #counts as ambiguous
      {return(0)}
    if(!$allow_align  && $sequence =~ /-/)   #not ambiguous
      {return(0)}
    if(!$allow_mask   && $sequence =~ /X/i)  #counts as ambiguous
      {return(0)}
    if($no_ambiguous  && $sequence =~ /[BDHVRYMKSWN]/i)
      {return(0)}

    #At this point, I know there's no non-DNA characters, so all I need to do
    #is find out the ratio of ambiguous characters (including X's and no .'s)
    my $num_ambig = scalar($sequence =~ /([BDHVRYKMSWNX\.])/ig);
    if($num_ambig)
      {
	my $tmp = $sequence;
	$sequence =~ s/[\-\s]//g; #Remove gaps and white space for length calc.
	my $len = length($tmp);
	return(-1) if($len == 0);
	return(($len - $num_ambig) / $len);
      }

    return(1);
  }

#Copied from fetch_cog.pl.pl on 8/6/2008 -Rob
sub getNextFastaRec
  {
#    my $self       = shift(@_);
    my $handle    = $_[0];      #File handle or file name
    my $no_format = $_[1];

    if(exists($main::{FASTABUFFER}) && exists($main::{FASTABUFFER}->{$handle}))
      {
	if(scalar(@{$main::{FASTABUFFER}->{$handle}}) > 0)
	  {
	    if(wantarray)
	      {
		my @array = (@{$main::{FASTABUFFER}->{$handle}});
		@{$main::{FASTABUFFER}->{$handle}} = ();
		return(@array);
	      }
	    return(shift(@{$main::{FASTABUFFER}->{$handle}}));
	  }
	elsif(eof($handle))
	  {return(undef)}
      }

    my $parent_id_check = {};
    my $first_loop = 0;
    my $line_num = 0;
    my $line     = '';
    my $defline  = '';
    my($seq);

    #For each line in the current input file
    while(getLine($handle))
      {
	$line_num = $.;
	$line = $_;

	next if($line !~ /\S/ || $line =~ /^\s*#/);
	if($line =~ />/)
	  {
	    if($defline)
	      {
		my $solidseq =
		  ($no_format ? $seq :
		   formatSequence($seq,0,undef,undef,undef,undef,undef,1));
		chomp($solidseq);
		chomp($defline);

		push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);
	      }
	    $defline = $line;

	    my $tmp_id = $defline;
	    $tmp_id =~ s/^\s*>\s*//;
	    $tmp_id =~ s/\s.*//;
	    if($tmp_id eq '')
	      {warning("No Defline ID on line: [$line_num] of current file.  ",
		       " Universal coordinates will be used if some were ",
		       "supplied either via command line arguments of via ",
		       "coordinate file with no parent sequence ID.")}
	    elsif(exists($parent_id_check->{$tmp_id}))
	      {
		error("Two sequences found with the same ID on the ",
		      "defline: [$tmp_id] in current fasta file.  The same ",
		      "pairs of coordinates will be used for each sequence.\n");
	      }

	    undef($seq);
	  }
	elsif($line =~ /^([^\t]+?) *\t\s*(.*)/)
	  {
	    $defline = $1;
	    $seq     = $2;

	    my $solidseq =
	      ($no_format ? $seq :
	       formatSequence($seq,0,undef,undef,undef,undef,undef,1));
	    chomp($solidseq);
	    chomp($defline);

	    push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);

	    undef($seq);
	  }
	else
	  {$seq .= $line}
      }

    #Handle the last sequence (if there were any sequences)
    if(defined($seq))
      {
	my $solidseq =
	  ($no_format ? $seq :
	   formatSequence($seq,0,undef,undef,undef,undef,undef,1));
	chomp($solidseq);
	chomp($defline);

	push(@{$main::{FASTABUFFER}->{$handle}},[$defline,$solidseq]);
      }

    #Return the first sequence (if sequence was parsed)
    if(exists($main::{FASTABUFFER}) && exists($main::{FASTABUFFER}->{$handle}))
      {
	if(scalar(@{$main::{FASTABUFFER}->{$handle}}) > 0)
	  {
	    if(wantarray)
	      {
		my @array = (@{$main::{FASTABUFFER}->{$handle}});
		@{$main::{FASTABUFFER}->{$handle}} = ();
		return(@array);
	      }
	    return(shift(@{$main::{FASTABUFFER}->{$handle}}));
	  }
	else
	  {return(undef)}
      }
    else
      {return(undef)}
  }

#Copied from fetch_cog.pl.pl on 8/6/2008 -Rob
sub formatSequence
  {
    #1. Read in the parameters.
    my $sequence          = $_[0];
    my $chars_per_line    = $_[1];
    my $coords_left_flag  = $_[2];
    my $coords_right_flag = $_[3];
    my $start_coord       = $_[4];
    my $coords_asc_flag   = $_[5];
    my $coord_upr_bound   = $_[6];
    my $uppercase_flag    = $_[7];
    my $print_flag        = $_[8];
    my $nucleotide_flag   = $_[9];

    my($formatted_sequence,
       $sub_string,
       $sub_sequence,
       $coord,
       $max_num_coord_digits,
       $line_size_left,
       $lead_spaces,
       $line);
    my $coord_separator = '  ';
    my $tmp_sequence = $sequence;
    $tmp_sequence =~ s/\s+//g;
    $tmp_sequence =~ s/<[^>]*>//g;
    my $seq_len = length($tmp_sequence);

    #2. Error check the parameters and set default values if unsupplied.
    my $default_chars_per_line    = ''; #Infinity
    my $default_coords_left_flag  = 0;
    my $default_coords_right_flag = 0;
    my $default_start_coord       = (!defined($coords_asc_flag) ||
				     $coords_asc_flag ? 1 : $seq_len);
    my $default_coords_asc_flag   = 1;
    my $default_coord_upr_bound   = undef();  #infinity (going past 1 produces
    my $default_uppercase_flag    = undef();  #          negative numbers)
    my $default_print_flag        = 0;

    if(!defined($chars_per_line) || $chars_per_line !~ /^\d+$/)
      {
        if(defined($chars_per_line) &&
	   $chars_per_line !~ /^\d+$/ && $chars_per_line =~ /./)
	  {print("WARNING:formatSequence.pl:formatSequence: Invalid ",
	         "chars_per_line: [$chars_per_line] - using default: ",
		 "[$default_chars_per_line]<BR>\n")}
        #end if(chars_per_line !~ /^\d+$/)
	$chars_per_line = $default_chars_per_line;
      }
    elsif(!$chars_per_line)
      {$chars_per_line = ''}
    #end if(!defined($chars_per_line) || $chars_per_line !~ /^\d+$/)
    if(!defined($coords_left_flag))
      {$coords_left_flag = $default_coords_left_flag}
    #end if(!defined(coords_left_flag))
    if(!defined($coords_right_flag))
      {$coords_right_flag = $default_coords_right_flag}
    #end if(!defined(coords_right_flag))
    if(!defined($start_coord) || $start_coord !~ /^\-?\d+$/)
      {
        if(defined($start_coord) && $start_coord !~ /^\d+$/ &&
	   $start_coord =~ /./ && ($coords_left_flag || $coords_right_flag))
          {print("WARNING:formatSequence.pl:formatSequence: Invalid ",
                 "start_coord: [$start_coord] - using default: ",
                 "[$default_start_coord]\n")}
        #end if($start_coord !~ /^\d+$/)
        $start_coord = $default_start_coord;
      }
    #end if(!defined($start_coord) || $start_coord !~ /^\d+$/)
    if(!defined($coords_asc_flag))
      {$coords_asc_flag = $default_coords_asc_flag}
    #end if(!defined(coords_right_flag))
    if(defined($coord_upr_bound) && $coord_upr_bound !~ /^\d+$/)
      {undef($coord_upr_bound)}
    if(!defined($print_flag))
      {$print_flag = $default_print_flag}
    #end if(!defined($print_flag))

    if(defined($coord_upr_bound) && $start_coord < 1)
      {$start_coord = $coord_upr_bound + $start_coord}
    elsif($start_coord < 1)
      {$start_coord--}
    elsif(defined($coord_upr_bound) && $start_coord > $coord_upr_bound)
      {$start_coord -= $coord_upr_bound}

    #3. Initialize the variables used for formatting.  (See the DATASTRUCTURES
    #   section.)
    if($coords_asc_flag)
      {
        if(defined($coord_upr_bound) &&
           ($seq_len + $start_coord) > $coord_upr_bound)
          {$max_num_coord_digits = length($coord_upr_bound)}
        else
          {$max_num_coord_digits = length($seq_len + $start_coord - 1)}

        $coord = $start_coord - 1;
      }
    else
      {
        if(defined($coord_upr_bound) && ($start_coord - $seq_len + 1) < 1)
          {$max_num_coord_digits = length($coord_upr_bound)}
        elsif(!defined($coord_upr_bound) &&
              length($start_coord - $seq_len - 1) > length($start_coord))
          {$max_num_coord_digits = length($start_coord - $seq_len - 1)}
        else
          {$max_num_coord_digits = length($start_coord)}

        $coord = $start_coord + 1;
      }
    $line_size_left = $chars_per_line;
    $lead_spaces    = $max_num_coord_digits - length($start_coord);

    #5. Add the first coordinate with spacing if coords_left_flag is true.
    $line = ' ' x $lead_spaces . $start_coord . $coord_separator
      if($coords_left_flag);

    #6. Foreach sub_string in the sequence where sub_string is either a
    #   sub_sequence or an HTML tag.
    foreach $sub_string (split(/(?=<)|(?<=>)/,$sequence))
      {
        #6.1 If the substring is an HTML tag
        if($sub_string =~ /^</)
          #6.1.1 Add it to the current line of the formatted_sequence
          {$line .= $sub_string}
        #end if(sub_string =~ /^</)
        #6.2 Else
        else
          {
            $sub_string =~ s/\s+//g;

	    if($nucleotide_flag)
	      {
		my(@errors);
		(@errors) = ($sub_string =~ /([^ATGCBDHVRYKMSWNX])/ig);
		$sub_string =~ s/([^ATGCBDHVRYKMSWNX])//ig;
		if(scalar(@errors))
		  {warning("[",scalar(@errors),"] bad nucleotide characters ",
			   "were filtered out of your sequence: [",
			   join('',@errors),"].\n")}
	      }

            #6.2.1 If the sequence is to be uppercased
            if(defined($uppercase_flag) && $uppercase_flag)
              #6.2.1.1 Uppercase the sub-string
              {$sub_string = uc($sub_string)}
            #end if(defined($uppercase_flag) && $uppercase_flag)
            #6.2.2 Else if the sequence is to be lowercased
            elsif(defined($uppercase_flag) && !$uppercase_flag)
              #6.2.2.1 Lowercase the sub-string
              {$sub_string = lc($sub_string)}
            #end elsif(defined($uppercase_flag) && !$uppercase_flag)

            #6.2.3 While we can grab enough sequence to fill the rest of a line
            while($sub_string =~ /(.{1,$line_size_left})/g)
              {
                $sub_sequence = $1;
                #6.2.3.1 Add the grabbed sequence to the current line of the
                #        formatted sequence
                $line .= $sub_sequence;
                #6.2.3.2 Increment the current coord by the amount of sequence
                #        grabbed
                my $prev_coord = $coord;
                if($coords_asc_flag)
                  {
                    $coord += length($sub_sequence);
                    if(defined($coord_upr_bound)      &&
                       $prev_coord <= $coord_upr_bound &&
                       $coord > $coord_upr_bound)
                      {$coord -= $coord_upr_bound}
                  }
                else
                  {
                    $coord -= length($sub_sequence);
                    if(defined($coord_upr_bound) &&
                       $prev_coord >= 1 && $coord < 1)
                      {$coord = $coord_upr_bound + $coord - 1}
                    elsif($prev_coord >= 1 && $coord < 1)
                      {$coord--}
                  }
                #6.2.3.3 If the length of the current sequence grabbed
                #        completes a line
                if((length($sub_sequence) == 0 && (!defined($line_size_left) ||
						   $line_size_left eq '')) ||
#		   (!defined($sub_sequence) && (!defined($line_size_left) ||
#						$line_size_left == 0)) ||
		   (length($sub_sequence) && defined($line_size_left) &&
		    $line_size_left ne '' &&
		    length($sub_sequence) == $line_size_left))
                  {
                    $lead_spaces = $max_num_coord_digits - length($coord);
                    #6.2.3.3.1 Conditionally add coordinates based on the
                    #          coords flags
                    $line .= $coord_separator . ' ' x $lead_spaces . $coord
                      if($coords_right_flag);

                    #6.2.3.3.2 Add a hard return to the current line of the
                    #          formatted sequence
                    $line .= "\n";

                    #6.2.3.3.3 Add the current line to the formatted_sequence
                    $formatted_sequence .= $line;
                    #6.2.3.3.4 Print the current line if the print_flag is true
                    print $line if($print_flag);

                    #6.2.3.3.5 Start the next line
                    $lead_spaces = $max_num_coord_digits - length($coord+1);
                    $line = '';
                    $line = ' ' x $lead_spaces
                          . ($coords_asc_flag ? ($coord+1) : ($coord-1))
                          . $coord_separator
                      if($coords_left_flag);

                    #6.2.3.3.6 Reset the line_size_left (length of remaining
                    #          sequence per line) to chars_per_line
                    $line_size_left = $chars_per_line;
                  }
                #end if(length($sub_sequence) == $line_size_left)
                #6.2.3.4 Else
                else
                  #6.2.3.4.1 Decrement line_size_left (length of remaining
                  #          sequence per line) by the amount of sequence
                  #          grabbed
                  {
		    if(!defined($line_size_left) || $line_size_left eq '')
		      {$line_size_left = -length($sub_sequence)}
		    else
		      {$line_size_left -= length($sub_sequence)}
		  }
                #end 6.2.3.4 Else
              }
            #end while($sub_string =~ /(.{1,$line_size_left})/g)
          }
        #end 6.2 Else
      }
    #end foreach $sub_string (split(/(?=<)|(?<=>)/,$sequence))
    #7. Add the last coodinate with enough leadin white-space to be lined up
    #   with the rest coordinates if the coords_right_flag is true
    $lead_spaces = $max_num_coord_digits - length($coord);
    $line .= ' ' x $line_size_left . $coord_separator . ' ' x $lead_spaces
          . $coord
      if($coords_right_flag && $line_size_left != $chars_per_line);
    $line =~ s/^\s*\d+$coord_separator\s*$// if($coords_left_flag);

    #8. Add the ending PRE tag to the last line of the formatted sequence
    $line =~ s/\n*$/\n/s;

    #9. Add the last line to the formatted_sequence
    $formatted_sequence .= $line;
    #10. Print the last line if the print_flag is true
    print $line if($print_flag);

    if($coord < 1 && ($coords_left_flag || $coords_right_flag))
      {print("WARNING: The sequence straddles the origin.  Coordinates are ",
             "inaccurate.")}

    #11. Return the formatted_sequence
    return $formatted_sequence;
  }

#Copied from fetch_cog.pl.pl on 8/6/2008 -Rob
sub reverseComplement
  {
    #1. Read in the sequence parameter.
#    my $self     = shift(@_);
    my $sequence = $_[0];
    return($sequence) if(!defined($sequence) || $sequence eq '');
    my @errors;
    if(@errors = ($sequence =~ /([^ATGCBVDHRYKMSWNatgcbvdhrykmswn\s\r])/isg))
      {error("Bad character(s) found: ['",join("','",@errors),"'].")}
    #end if(@errors = ($sequence =~ /([^ATGCBVDHRYKM\s\r])/isg))
    #2. Transcribe the new_sequence.
    $sequence =~ tr/ATGCBVDHRYKMatgcbvdhrykm/TACGVBHDYRMKtacgvbhdyrmk/;
    #3. Reverse the new_sequence.
    $sequence = reverse($sequence);
    return $sequence;
  }

#Copied from fetch_cog.pl.pl on 8/6/2008 -Rob
#Edited
sub translate
  {
    # Input Arguements:
    # 1.  Gene Nucleotide Sequence
    # 2.  Genetic Code
    # Return Value
    # 1.  An amino acid string if all goes well.
    # 2.  Empty string if something fails
    if($_[0] !~ /\w/)
      {
	error("Invalid gene sequence: [$_[0]] passed in.\n");
	return($_[2] ? [] : '');
      }
    if($_[1] !~ /\d/)
      {
	error("Invalid genetic code: [$_[1]] passed in.\n");
	return($_[2] ? [] : '');
      }

    my %codon_table;
    my $codon;
    my @sequence;
    my @protein_sequences;
    my @gene_sequences;
    my @gene_coords = ();
    my $tmp_pro_seq;
    my $nucleotide_sequence  = $_[0];
    my $genetic_code         = $_[1];
    my $use_starts_and_stops = $_[2]; #Selectively translates between all valid
                                      #starts and stops in the frame the
                                      #sequence is sent in with (starting from
                                      #1)
    my $translate_ends       = $_[3]; #Only used if $use_starts_and_stops=true
                                      #Translates from the beginning to the
                                      #first stop codon and off the end unless
                                      #a stop is encountered
    my $starts               = $_[4] ? $_[4] : {atg=>1,gtg=>1,ttg=>1,
						  rtg=>1,dtg=>1,ktg=>1,wtg=>1};
    my $stop_char            = $_[5] ? $_[5] : '*';
    my $ambig_char           = $_[6] ? $_[6] : 'X';
    my $min_full_length      = defined($_[7]) ? $_[7] : 0;
    my $min_part_length      = defined($_[8]) ? $_[8] : $min_full_length;
    my $length               = length($nucleotide_sequence);
    my $i;
    debug("Min part length sent in: [$_[8]] and set to: [$min_part_length].");

    my $trim_length = $length % 3;
    my $tack_back   = '';
    if($trim_length)
      {
	if($nucleotide_sequence =~ /(.{$trim_length})$/)
	  {
	    $tack_back = uc($1);
	    $tack_back =~ s/x+$//ig;
	  }
	$nucleotide_sequence =~ s/.{$trim_length}$//;
	$length = length($nucleotide_sequence);
      }

    # build the hash codon table
    %codon_table = ('ttt' => 'F','ttc' => 'F','tty' => 'F',
		    'tgg' => 'W',
		    'tga' => (($genetic_code == 4) ? 'W' : $stop_char),
		    'tta' => 'L','ttg' => 'L','ttr' => 'L',
		    'tct' => 'S','tcc' => 'S','tca' => 'S','tcg' => 'S',
		    'agt' => 'S','agc' => 'S','agy' => 'S',
		    'tcn' => 'S','tcb' => 'S','tcd' => 'S','tch' => 'S',
		    'tcv' => 'S','tcr' => 'S','tcy' => 'S','tck' => 'S',
		    'tcm' => 'S','tcs' => 'S','tcw' => 'S',
		    'tat' => 'Y','tac' => 'Y','tay' => 'Y',
		    'tgt' => 'C','tgc' => 'C','tgy' => 'C',
		    'ctt' => 'L','ctc' => 'L','cta' => 'L','ctg' => 'L',
		    'ctn' => 'L','ctb' => 'L','ctd' => 'L','cth' => 'L',
		    'ctv' => 'L','ctr' => 'L','cty' => 'L','ctk' => 'L',
		    'ctm' => 'L','cts' => 'L','ctw' => 'L',
		    'cct' => 'P','ccc' => 'P','cca' => 'P','ccg' => 'P',
		    'ccn' => 'P','ccb' => 'P','ccd' => 'P','cch' => 'P',
		    'ccv' => 'P','ccr' => 'P','ccy' => 'P','cck' => 'P',
		    'ccm' => 'P','ccs' => 'P','ccw' => 'P',
		    'cat' => 'H','cac' => 'H','cay' => 'H',
		    'caa' => 'Q','cag' => 'Q','car' => 'Q',
		    'cgt' => 'R','cgc' => 'R','cga' => 'R','cgg' => 'R',
		    'aga' => 'R','agg' => 'R','agr' => 'R',
		    'cgn' => 'R','cgb' => 'R','cgd' => 'R','cgh' => 'R',
		    'cgv' => 'R','cgr' => 'R','cgy' => 'R','cgk' => 'R',
		    'cgm' => 'R','cgs' => 'R','cgw' => 'R',
		    'mga' => 'R','mgg' => 'R','mgr' => 'R',
		    'att' => 'I','atc' => 'I','ata' => 'I','ath' => 'I',
		    'aty' => 'I','atw' => 'I','atm' => 'I',
		    'atg' => 'M',
		    'act' => 'T','acc' => 'T','aca' => 'T','acg' => 'T',
		    'acn' => 'T','acb' => 'T','acd' => 'T','ach' => 'T',
		    'acv' => 'T','acr' => 'T','acy' => 'T','ack' => 'T',
		    'acm' => 'T','acs' => 'T','acw' => 'T',
		    'aat' => 'N','aac' => 'N','aay' => 'N',
		    'aaa' => 'K','aag' => 'K','aar' => 'K',
		    'gtt' => 'V','gtc' => 'V','gta' => 'V','gtg' => 'V',
		    'gtn' => 'V','gtb' => 'V','gtd' => 'V','gth' => 'V',
		    'gtv' => 'V','gtr' => 'V','gty' => 'V','gtk' => 'V',
		    'gtm' => 'V','gts' => 'V','gtw' => 'V',
		    'gct' => 'A','gcc' => 'A','gca' => 'A','gcg' => 'A',
		    'gcn' => 'A','gcb' => 'A','gcd' => 'A','gch' => 'A',
		    'gcv' => 'A','gcr' => 'A','gcy' => 'A','gck' => 'A',
		    'gcm' => 'A','gcs' => 'A','gcw' => 'A',
		    'gat' => 'D','gac' => 'D','gay' => 'D',
		    'gaa' => 'E','gag' => 'E','gar' => 'E',
		    'ggt' => 'G','ggc' => 'G','gga' => 'G','ggg' => 'G',
		    'ggn' => 'G','ggb' => 'G','ggd' => 'G','ggh' => 'G',
		    'ggv' => 'G','ggr' => 'G','ggy' => 'G','ggk' => 'G',
		    'ggm' => 'G','ggs' => 'G','ggw' => 'G',
		    'taa' => $stop_char,'tag' => $stop_char,
		    'tar' => $stop_char);
    $codon_table{tra} = $stop_char if($genetic_code != 4);

    chomp $nucleotide_sequence;
    chomp $genetic_code;
    $nucleotide_sequence =~ tr/A-Z/a-z/;
    @sequence = split(//, $nucleotide_sequence);
    $length = @sequence;

    my @bad_codons           = ();
    my $in_coding_region     = $translate_ends;
    my $seq_index            = 0;
    my $num_stops_hit        = 0;
    my $first_real_aa_seen   = 0;
    my $first_seq_is_partial = 0;
    my $last_seq_is_partial  = 0;

    for($i = 0; $i < $length; $i = $i+3)
      {
	$codon = $sequence[$i] . $sequence[$i+1] . $sequence[$i+2];
	$codon = lc($codon);

	if(!$first_real_aa_seen && exists($codon_table{$codon}))
	  {
	    $first_real_aa_seen = 1;

	    #Note a sequence is considered partial if we have not hit a stop
	    #yet and this is the first real aa seen because even if the first
	    #codon may be a start, the real start could be off the end
	    #Later on, we'll check to see if no stops have been hit, then it
	    #will not be treated as partial
	    if($num_stops_hit == 0)
	      {$first_seq_is_partial = 1}
	  }

	#See if we've hit a start codon
	if($use_starts_and_stops &&
	   exists($starts->{$codon}) && !$in_coding_region)
	  {
	    $seq_index++ if(scalar(@protein_sequences) == ($seq_index + 1) &&
			    defined($protein_sequences[$seq_index]) &&
			    $protein_sequences[$seq_index] ne '');
	    $in_coding_region = 1;
	  }

    	if(exists($codon_table{$codon}))
	  {
	    if($use_starts_and_stops)
	      {
		if($codon_table{$codon} eq $stop_char)
		  {
		    $num_stops_hit++
		      if($in_coding_region &&
			 defined($protein_sequences[$seq_index]) &&
			 $protein_sequences[$seq_index] ne '');
		    $in_coding_region = 0;
		  }
		next if(!$in_coding_region);
	      }
	    $protein_sequences[$seq_index] .= $codon_table{$codon};
	    $gene_sequences[$seq_index] .= uc($codon);

	    #Add the start or update the stop
	    if(length($gene_sequences[$seq_index]) == 3)
	      {
		$gene_coords[$seq_index]->{START} = $i + 1;
		$gene_coords[$seq_index]->{STOP} = $i + length($codon);
	      }
	    else
	      {$gene_coords[$seq_index]->{STOP} = $i + length($codon)}
	  }
	else
	  {
	    next if($use_starts_and_stops && !$in_coding_region);
	    push(@bad_codons,$codon);

	    #This conditional serves to trim X's off the beginning of a partial
	    #sequence
	    if($first_real_aa_seen)
	      {
		$protein_sequences[$seq_index] .= $ambig_char;
		$gene_sequences[$seq_index] .= uc($codon);

		#Add the start or update the stop
		if(length($gene_sequences[$seq_index]) == 3)
		  {
		    $gene_coords[$seq_index]->{START} = $i + 1;
		    $gene_coords[$seq_index]->{STOP} = $i + length($codon);
		  }
		else
		  {$gene_coords[$seq_index]->{STOP} = $i + length($codon)}
	      }
	  }
      }

    #If we're translating the ends and we translated off the end (i.e. we're
    #still in a coding region)
    if($use_starts_and_stops && $translate_ends && $in_coding_region)
      {
	#Trim X's off the end of a partial sequence
	if($protein_sequences[$#protein_sequences] =~ /(x+)$/i)
	  {
	    my $xs = $1;
	    my $trim_len = length($xs) * 3;
	    $protein_sequences[$#protein_sequences] =~ s/x+$//i;
	    $gene_sequences[$#gene_sequences] =~ s/.{$trim_len}$//;
	    $gene_coords[$#gene_sequences]->{STOP} -= $trim_len;
	  }

	$last_seq_is_partial = 1;
      }

    debug("Sequences before imposing length restrictions:\n",
	  join("\n",@protein_sequences),"\n");

    if($use_starts_and_stops && !$translate_ends)
      {
	$codon = $sequence[0] . $sequence[1] . $sequence[2];
	#If (the first codon was not a start codon or there's more than 1
	#sequence) and the number of stops is 1 less than the number of
	#sequences
	if((!exists($starts->{$codon}) || scalar(@protein_sequences) > 1) &&
	   $num_stops_hit < scalar(@protein_sequences))
	  {
	    pop(@protein_sequences);
	    pop(@gene_sequences);
	    pop(@gene_coords);
	  }

	#Impose the full length restriction
#	@protein_sequences =
#	  grep {my $s = $_;$s=~s/x//ig;length($s) >= $min_full_length}
#	    @protein_sequences;
	my @good_indexes =
	  grep {my $s = $protein_sequences[$_];$s=~s/x//ig;
		length($s) >= $min_full_length}
	    (0..(scalar(@protein_sequences)-1));
	my @new_protein_sequences = map {$protein_sequences[$_]} @good_indexes;
	@protein_sequences = @new_protein_sequences;
	my @new_gene_sequences = map {$gene_sequences[$_]} @good_indexes;
	@gene_sequences = @new_gene_sequences;
	my @new_gene_coords = map {$gene_coords[$_]} @good_indexes;
	@gene_coords = @new_gene_coords;
      }

    #Impose minimum lengths restrictions
    if($use_starts_and_stops && $translate_ends &&
       $min_full_length != $min_part_length)
      {
	$codon = $sequence[0] . $sequence[1] . $sequence[2];
	debug("First Codon: [$codon].");
	#If there's only 1 sequence and it has a start, assume a stop is there
	if(exists($starts->{$codon}) && scalar(@protein_sequences) == 1 &&
	   $num_stops_hit == 0)
	  {
	    debug("Start exists, there's only 1 sequence, and the number of ",
		  "stops is 0.");
	    my $s = $protein_sequences[0];
	    $s =~ s/x//ig;
	    if(length($s) < $min_full_length)
	      {
		pop(@protein_sequences);
		pop(@gene_sequences);
		pop(@gene_coords);
	      }
	  }
	else
	  {
	    #Store the number of sequences
	    my $num_sequences = scalar(@protein_sequences);
	    #Grab the first partial sequence
	    my($part1,$part1g,$part1c);
	    if(scalar(@protein_sequences) && $first_seq_is_partial)
	      {
		$part1  = shift(@protein_sequences);
		$part1g = shift(@gene_sequences);
		$part1c = shift(@gene_coords);
	      }
	    #Grab the last partial sequence
	    my($part2,$part2g,$part2c);
	    if(scalar(@protein_sequences) && $last_seq_is_partial)
	      {
		$part2  = pop(@protein_sequences);
		$part2g = pop(@gene_sequences);
		$part2c = pop(@gene_coords);

		#If there was sequence trimmed off, let's put it back onto the
		#partial sequence at the 3' end
		if($tack_back ne '')
		  {
		    $part2g         .= $tack_back;
		    $part2c->{STOP} += length($tack_back);
		  }
	      }

	    #Impose the full length restriction
#	    @protein_sequences =
#	      grep {my $s = $_;$s =~ s/x//ig;length($s) >= $min_full_length}
#		@protein_sequences;
	    my @good_indexes =
	      grep {my $s = $protein_sequences[$_];$s=~s/x//ig;
		    length($s) >= $min_full_length}
		(0..(scalar(@protein_sequences)-1));
	    my @new_protein_sequences = map {$protein_sequences[$_]}
	      @good_indexes;
	    @protein_sequences = @new_protein_sequences;
	    my @new_gene_sequences = map {$gene_sequences[$_]} @good_indexes;
	    @gene_sequences = @new_gene_sequences;
	    my @new_gene_coords = map {$gene_coords[$_]} @good_indexes;
	    @gene_coords = @new_gene_coords;

	    #Impose the partial length restriction
	    my $s = $part2;
	    $s =~ s/x//ig if(defined($s));
	    if(defined($part2) && length($s) >= $min_part_length)
	      {
		debug("Last sequence: [$part2] meets partial minimum length: ",
		      "[",length($s)," >= $min_part_length].");
		push(@protein_sequences,$part2);
		push(@gene_sequences,$part2g);
		push(@gene_coords,$part2c);
	      }
	    elsif(defined($part2))
	      {
		$last_seq_is_partial = 0;
		debug("Last sequence: [$part2] does not meet partial minimum ",
		      "length: [",length($s)," >= $min_part_length].");
	      }
	    $s = $part1;
	    $s =~ s/x//ig if(defined($s));
	    if(defined($part1) && length($s) >= $min_part_length)
	      {
		debug("First sequence: [$part1 -> $s] meets partial minimum ",
		      "length: [",length($s)," >= $min_part_length].");
		unshift(@protein_sequences,$part1);
		unshift(@gene_sequences,$part1g);
		unshift(@gene_coords,$part1c);
	      }
	    elsif(defined($part1))
	      {
		$first_seq_is_partial = 0;
		debug("First sequence: [$part1] does not meet partial ",
		      "minimum length: [",length($s)," >= $min_part_length].");
	      }
	  }
      }
    elsif($use_starts_and_stops && $translate_ends)
      {
	#Impose the full length restriction
#	@protein_sequences =
#	  grep {my $s = $_;$s =~ s/x//ig;length($s) >= $min_full_length}
#	    @protein_sequences;
	my @good_indexes =
	  grep {my $s = $protein_sequences[$_];$s=~s/x//ig;
		length($s) >= $min_full_length}
	    (0..(scalar(@protein_sequences)-1));
	my @new_protein_sequences = map {$protein_sequences[$_]} @good_indexes;
	@protein_sequences = @new_protein_sequences;
	my @new_gene_sequences = map {$gene_sequences[$_]} @good_indexes;
	@gene_sequences = @new_gene_sequences;
	my @new_gene_coords = map {$gene_coords[$_]} @good_indexes;
	@gene_coords = @new_gene_coords;
      }

    debug("Sequences after imposing length restrictions:\n",
	  join("\n",@protein_sequences),"\n");

    if(scalar(@bad_codons))
      {warning("Unrecognized or ambiguous codons found in your sequence: ",
	       "[@bad_codons].  Inserting $ambig_char\'s (for any amino ",
	       "acid).")}

    if($use_starts_and_stops)
      {return([@protein_sequences],[@gene_sequences],[@gene_coords],
	      $first_seq_is_partial,$last_seq_is_partial)}
    return(@protein_sequences);
  }

##
## This subroutine prints a description of the script and it's input and output
## files.
##
sub help
  {
    my $script = $0;
    my $lmd = localtime((stat($script))[9]);
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Print a description of this program
    print << "end_print";

$script
Copyright 2007
Robert W. Leach
Created on DATE HERE
Last Modified on $lmd
Center for Computational Research
701 Ellicott Street
Buffalo, NY 14203
rwleach\@ccr.buffalo.edu

* WHAT IS THIS: Performs a 6-frame translation of necleotide sequences.

* INPUT FORMAT: Fasta format.

* OUTPUT FORMAT: Fasta format.

end_print

    return(0);
  }

##
## This subroutine prints a usage statement in long or short form depending on
## whether "no descriptions" is true.
##
sub usage
  {
    my $no_descriptions = $_[0];

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Grab the first version of each option from the global GetOptHash
    my $options = '[' .
      join('] [',
	   grep {$_ ne '-i'}        #Remove REQUIRED params
	   map {my $key=$_;         #Save the key
		$key=~s/\|.*//;     #Remove other versions
		$key=~s/(\!|=.)$//; #Remove trailing getopt stuff
		$key = (length($key) > 1 ? '--' : '-') . $key;} #Add dashes
	   grep {$_ ne '<>'}        #Remove the no-flag parameters
	   keys(%$GetOptHash)) .
	     ']';

    print << "end_print";
USAGE: $script -i "input file(s)" $options
       $script $options < input_file
end_print

    if($no_descriptions)
      {print("Execute $script with no options to see a description of the ",
             "available parameters.\n")}
    else
      {
        print << 'end_print';

     -i|--fasta-file*     REQUIRED Space-separated input file(s inside quotes).
                                   *No flag required.  Standard input via
                                   redirection is acceptable.  Perl glob
                                   characters (e.g. '*') are acceptable inside
                                   quotes.
     -s|--stop-character  OPTIONAL [*] The character to represent stop codons.
     -a|--ambig-character OPTIONAL [X] The character to represent unknown amino
                                   acids.
     -g|--genetic-code    OPTIONAL [11] The IUPAC genetic code to use in
                                   translating the DNA to protein.
     -o|--outfile-suffix  OPTIONAL [nothing] This suffix is added to the input
                                   file names to use as output files.
                                   Redirecting a file into this script will
                                   result in the output file name to be "STDIN"
                                   with your suffix appended.
     --dna-suffix         OPTIONAL [none] An extension to the input file name
                                   to use for output DNA files containing the
                                   ORF sequences parsed from the submitted
                                   sequence.  Only used if -v is supplied.
     -d|--do-not-append-  OPTIONAL [Off] Do not append the frame or ORF ID to
        to-id                      the input sequence ID.  Note, this will
                                   cause multiple sequences to have the same
                                   ID.
     -v|--valid-seqs-only OPTIONAL [Off] Do not translate the sequence from
                                   end-to-end, but rather only translate
                                   sequences which have a valid start and stop
                                   codon.
     -p|--allow-partials  OPTIONAL [Off] When outputting valid sequences (-v),
                                   include translations of the ends of the
                                   sequences up to the first stop or from the
                                   last start.  Only used when -v is supplied.
     -f|--min-full-length OPTIONAL [0] When translating valid sequences, skip
                                   sequences which are shorter than this length
                                   (measured in amino acids).  Only used if -v
                                   is supplied.
     -r|--min-part-length OPTIONAL [0] When translating partial valid sequences
                                   (see -p), skip sequences which are shorter
                                   than this length (measured in amino acids).
                                   Only used if -p is supplied.
     --force              OPTIONAL [Off] Force overwrite of existing output
                                   files (generated from previous runs of this
                                   script).  Only used when the -o option is
                                   supplied.
     --verbose            OPTIONAL [Off] Verbose mode.  Cannot be used with the
                                   quiet flag.
     --quiet              OPTIONAL [Off] Quiet mode.  Turns off warnings and
                                   errors.  Cannot be used with the verbose
                                   flag.
     --nowarn             OPTIONAL [Off] semi-quiet mode.  Turns off warnings.
                                   May be used with the verbose flag.
     -h|--help            OPTIONAL [Off] Help.  Use this option to see an
                                   explanation of the script and its input and
                                   output files.
     --version            OPTIONAL [Off] Print software version number.  If
                                   verbose mode is on, it also prints the
                                   template version used to standard error.
     --debug              OPTIONAL [Off] Debug mode.

end_print
      }

    return(0);
  }


##
## Subroutine that prints formatted verbose messages.  Specifying a 1 as the
## first argument prints the message in overwrite mode (meaning subsequence
## verbose, error, warning, or debug messages will overwrite the message
## printed here.  However, specifying a hard return as the first character will
## override the status of the last line printed and keep it.  Global variables
## keep track of print length so that previous lines can be cleanly
## overwritten.
##
sub verbose
  {
    return(0) unless($verbose);

    #Read in the first argument and determine whether it's part of the message
    #or a value for the overwrite flag
    my $overwrite_flag = $_[0];

    #If a flag was supplied as the first parameter (indicated by a 0 or 1 and
    #more than 1 parameter sent in)
    if(scalar(@_) > 1 && ($overwrite_flag eq '0' || $overwrite_flag eq '1'))
      {shift(@_)}
    else
      {$overwrite_flag = 0}

    #Ignore the overwrite flag if STDOUT will be mixed in
    $overwrite_flag = 0 if(isStandardOutputToTerminal());

    #Read in the message
    my $verbose_message = join('',grep {defined($_)} @_);

    $overwrite_flag = 1 if(!$overwrite_flag && $verbose_message =~ /\r/);

    #Initialize globals if not done already
    $main::last_verbose_size  = 0 if(!defined($main::last_verbose_size));
    $main::last_verbose_state = 0 if(!defined($main::last_verbose_state));
    $main::verbose_warning    = 0 if(!defined($main::verbose_warning));

    #Determine the message length
    my($verbose_length);
    if($overwrite_flag)
      {
	$verbose_message =~ s/\r$//;
	if(!$main::verbose_warning && $verbose_message =~ /\n|\t/)
	  {
	    warning("Hard returns and tabs cause overwrite mode to not work ",
		    "properly.");
	    $main::verbose_warning = 1;
	  }
      }
    else
      {chomp($verbose_message)}

    if(!$overwrite_flag)
      {$verbose_length = 0}
    elsif($verbose_message =~ /\n([^\n]*)$/)
      {$verbose_length = length($1)}
    else
      {$verbose_length = length($verbose_message)}

    #Overwrite the previous verbose message by appending spaces just before the
    #first hard return in the verbose message IF THE VERBOSE MESSAGE DOESN'T
    #BEGIN WITH A HARD RETURN.  However note that the length stored as the
    #last_verbose_size is the length of the last line printed in this message.
    if($verbose_message =~ /^([^\n]*)/ && $main::last_verbose_state &&
       $verbose_message !~ /^\n/)
      {
	my $append = ' ' x ($main::last_verbose_size - length($1));
	unless($verbose_message =~ s/\n/$append\n/)
	  {$verbose_message .= $append}
      }

    #If you don't want to overwrite the last verbose message in a series of
    #overwritten verbose messages, you can begin your verbose message with a
    #hard return.  This tells verbose() to not overwrite the last line that was
    #printed in overwrite mode.

    #Print the message to standard error
    print STDERR ($verbose_message,
		  ($overwrite_flag ? "\r" : "\n"));

    #Record the state
    $main::last_verbose_size  = $verbose_length;
    $main::last_verbose_state = $overwrite_flag;

    #Return success
    return(0);
  }

sub verboseOverMe
  {verbose(1,@_)}

##
## Subroutine that prints errors with a leading program identifier containing a
## trace route back to main to see where all the subroutine calls were from,
## the line number of each call, an error number, and the name of the script
## which generated the error (in case scripts are called via a system call).
##
sub error
  {
    return(0) if($quiet);

    #Gather and concatenate the error message and split on hard returns
    my @error_message = split("\n",join('',grep {defined($_)} @_));
    pop(@error_message) if($error_message[-1] !~ /\S/);

    $main::error_number++;

    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Assign the values from the calling subroutines/main
    my @caller_info = caller(0);
    my $line_num = $caller_info[2];
    my $caller_string = '';
    my $stack_level = 1;
    while(@caller_info = caller($stack_level))
      {
	my $calling_sub = $caller_info[3];
	$calling_sub =~ s/^.*?::(.+)$/$1/ if(defined($calling_sub));
	$calling_sub = (defined($calling_sub) ? $calling_sub : 'MAIN');
	$caller_string .= "$calling_sub(LINE$line_num):"
	  if(defined($line_num));
	$line_num = $caller_info[2];
	$stack_level++;
      }
    $caller_string .= "MAIN(LINE$line_num):";

    my $leader_string = "ERROR$main::error_number:$script:$caller_string ";

    #Figure out the length of the first line of the error
    my $error_length = length(($error_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $error_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@error_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $error_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## Subroutine that prints warnings with a leader string containing a warning
## number
##
sub warning
  {
    return(0) if($quiet || $nowarn);

    $main::warning_number++;

    #Gather and concatenate the warning message and split on hard returns
    my @warning_message = split("\n",join('',grep {defined($_)} @_));
    pop(@warning_message) if($warning_message[-1] !~ /\S/);

    my $leader_string = "WARNING$main::warning_number: ";

    #Figure out the length of the first line of the error
    my $warning_length = length(($warning_message[0] =~ /\S/ ?
				 $leader_string : '') .
				$warning_message[0]);

    #Put leader string at the beginning of each line of the message
    foreach my $line (@warning_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $warning_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## Subroutine that gets a line of input and accounts for carriage returns that
## many different platforms use instead of hard returns.  Note, it uses a
## global array reference variable ($infile_line_buffer) to keep track of
## buffered lines from multiple file handles.
##
sub getLine
  {
    my $file_handle = $_[0];

    #Set a global array variable if not already set
    $main::infile_line_buffer = {} if(!defined($main::infile_line_buffer));
    if(!exists($main::infile_line_buffer->{$file_handle}))
      {$main::infile_line_buffer->{$file_handle}->{FILE} = []}

    #If this sub was called in array context
    if(wantarray)
      {
	#Check to see if this file handle has anything remaining in its buffer
	#and if so return it with the rest
	if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) > 0)
	  {
	    return(@{$main::infile_line_buffer->{$file_handle}->{FILE}},
		   map
		   {
		     #If carriage returns were substituted and we haven't
		     #already issued a carriage return warning for this file
		     #handle
		     if(s/\r\n|\n\r|\r/\n/g &&
			!exists($main::infile_line_buffer->{$file_handle}
				->{WARNED}))
		       {
			 $main::infile_line_buffer->{$file_handle}->{WARNED}
			   = 1;
			 warning("Carriage returns were found in your file ",
				 "and replaced with hard returns");
		       }
		     split(/(?<=\n)/,$_);
		   } <$file_handle>);
	  }
	
	#Otherwise return everything else
	return(map
	       {
		 #If carriage returns were substituted and we haven't already
		 #issued a carriage return warning for this file handle
		 if(s/\r\n|\n\r|\r/\n/g &&
		    !exists($main::infile_line_buffer->{$file_handle}
			    ->{WARNED}))
		   {
		     $main::infile_line_buffer->{$file_handle}->{WARNED}
		       = 1;
		     warning("Carriage returns were found in your file ",
			     "and replaced with hard returns");
		   }
		 split(/(?<=\n)/,$_);
	       } <$file_handle>);
      }

    #If the file handle's buffer is empty, put more on
    if(scalar(@{$main::infile_line_buffer->{$file_handle}->{FILE}}) == 0)
      {
	my $line = <$file_handle>;
	if(!eof($file_handle))
	  {
	    if($line =~ s/\r\n|\n\r|\r/\n/g &&
	       !exists($main::infile_line_buffer->{$file_handle}->{WARNED}))
	      {
		$main::infile_line_buffer->{$file_handle}->{WARNED} = 1;
		warning("Carriage returns were found in your file and ",
			"replaced with hard returns");
	      }
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} =
	      split(/(?<=\n)/,$line);
	  }
	else
	  {
	    #Do the \r substitution for the last line of files that have the
	    #eof character at the end of the last line instead of on a line by
	    #itself.  I tested this on a file that was causing errors for the
	    #last line and it works.
	    $line =~ s/\r/\n/g if(defined($line));
	    @{$main::infile_line_buffer->{$file_handle}->{FILE}} = ($line);
	  }
      }

    #Shift off and return the first thing in the buffer for this file handle
    return($_ = shift(@{$main::infile_line_buffer->{$file_handle}->{FILE}}));
  }

##
## This subroutine allows the user to print debug messages containing the line
## of code where the debug print came from and a debug number.  Debug prints
## will only be printed (to STDERR) if the debug option is supplied on the
## command line.
##
sub debug
  {
    return(0) unless($DEBUG);

    $main::debug_number++;

    #Gather and concatenate the error message and split on hard returns
    my @debug_message = split("\n",join('',grep {defined($_)} @_));
    pop(@debug_message) if($debug_message[-1] !~ /\S/);

    #Assign the values from the calling subroutine
    #but if called from main, assign the values from main
    my($junk1,$junk2,$line_num,$calling_sub);
    (($junk1,$junk2,$line_num,$calling_sub) = caller(1)) ||
      (($junk1,$junk2,$line_num) = caller());

    #Edit the calling subroutine string
    $calling_sub =~ s/^.*?::(.+)$/$1:/ if(defined($calling_sub));

    my $leader_string = "DEBUG$main::debug_number:LINE$line_num:" .
      (defined($calling_sub) ? $calling_sub : '') .
	' ';

    #Figure out the length of the first line of the error
    my $debug_length = length(($debug_message[0] =~ /\S/ ?
			       $leader_string : '') .
			      $debug_message[0]);

    #Put location information at the beginning of each line of the message
    foreach my $line (@debug_message)
      {print STDERR (($line =~ /\S/ ? $leader_string : ''),
		     $line,
		     ($verbose &&
		      defined($main::last_verbose_state) &&
		      $main::last_verbose_state ?
		      ' ' x ($main::last_verbose_size - $debug_length) : ''),
		     "\n")}

    #Reset the verbose states if verbose is true
    if($verbose)
      {
	$main::last_verbose_size = 0;
	$main::last_verbose_state = 0;
      }

    #Return success
    return(0);
  }


##
## This sub marks the time (which it pushes onto an array) and in scalar
## context returns the time since the last mark by default or supplied mark
## (optional) In array context, the time between all marks is always returned
## regardless of a supplied mark index
## A mark is not made if a mark index is supplied
## Uses a global time_marks array reference
##
sub markTime
  {
    #Record the time
    my $time = time();

    #Set a global array variable if not already set to contain (as the first
    #element) the time the program started (NOTE: "$^T" is a perl variable that
    #contains the start time of the script)
    $main::time_marks = [$^T] if(!defined($main::time_marks));

    #Read in the time mark index or set the default value
    my $mark_index = (defined($_[0]) ? $_[0] : -1);  #Optional Default: -1

    #Error check the time mark index sent in
    if($mark_index > (scalar(@$main::time_marks) - 1))
      {
	error("Supplied time mark index is larger than the size of the ",
	      "time_marks array.\nThe last mark will be set.");
	$mark_index = -1;
      }

    #Calculate the time since the time recorded at the time mark index
    my $time_since_mark = $time - $main::time_marks->[$mark_index];

    #Add the current time to the time marks array
    push(@$main::time_marks,$time)
      if(!defined($_[0]) || scalar(@$main::time_marks) == 0);

    #If called in array context, return time between all marks
    if(wantarray)
      {
	if(scalar(@$main::time_marks) > 1)
	  {return(map {$main::time_marks->[$_ - 1] - $main::time_marks->[$_]}
		  (1..(scalar(@$main::time_marks) - 1)))}
	else
	  {return(())}
      }

    #Return the time since the time recorded at the supplied time mark index
    return($time_since_mark);
  }

##
## This subroutine reconstructs the command entered on the command line
## (excluding standard input and output redirects).  The intended use for this
## subroutine is for when a user wants the output to contain the input command
## parameters in order to keep track of what parameters go with which output
## files.
##
sub getCommand
  {
    my $perl_path_flag = $_[0];
    my($command);

    #Determine the script name
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;

    #Put quotes around any parameters containing un-escaped spaces or astericks
    my $arguments = [@$preserve_args];
    foreach my $arg (@$arguments)
      {if($arg =~ /(?<!\\)[\s\*]/ || $arg eq '')
	 {$arg = "'" . $arg . "'"}}

    #Determine the perl path used (dependent on the `which` unix built-in)
    if($perl_path_flag)
      {
	$command = `which $^X`;
	chomp($command);
	$command .= ' ';
      }

    #Build the original command
    $command .= join(' ',($0,@$arguments));

    #Note, this sub doesn't add any redirected files in or out

    return($command);
  }

##
## This subroutine checks to see if a parameter is a single file with spaces in
## the name before doing a glob (which would break up the single file name
## improperly).  The purpose is to allow the user to enter a single input file
## name using double quotes and un-escaped spaces as is expected to work with
## many programs which accept individual files as opposed to sets of files.  If
## the user wants to enter multiple files, it is assumed that space delimiting
## will prompt the user to realize they need to escape the spaces in the file
## names.
##
sub sglob
  {
    my $command_line_string = $_[0];
    return(-e $command_line_string ?
	   $command_line_string : glob($command_line_string));
  }


sub printVersion
  {
    my $script = $0;
    $script =~ s/^.*\/([^\/]+)$/$1/;
    print(($verbose ? "$script Version " : ''),
	  $software_version_number,
	  "\n");
    verbose("Generated using perl_script_template.pl\n",
	    "Version $template_version_number\n",
	    "Robert W. Leach\n",
	    "robleach\@ccr.buffalo.edu\n",
	    "5/8/2006\n",
	    "Center for Computational Research\n",
	    "Copyright 2008");
    return(0);
  }

#This subroutine is a check to see if input is user-entered via a TTY (result
#is non-zero) or directed in (result is zero)
sub isStandardInputFromTerminal
  {return(-t STDIN || eof(STDIN))}

#This subroutine is a check to see if prints are going to a TTY.  Note,
#explicit prints to STDOUT when another output handle is selected are not
#considered and may defeat this subroutine.
sub isStandardOutputToTerminal
  {return(-t STDOUT && select() eq 'main::STDOUT')}
