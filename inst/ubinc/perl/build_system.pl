# ! /usr/bin/perl
#
# JMH 
# in system_check, look for repeated set elements (like A below)
#  <SET:MYSET> A ; B ; C ; A ; D
# Check: CVSET -> when adding a covariate set make sure the set is already
# define with a Pset command
#
# NONMEM Target:
#  I used the fortran functions and conditional format, this may be wrong, but
#  the examples I have look fortranny 
#
#   Number of states:  scalar(@{$cfg->{species_index}}) 
#
# ---------------------
#  Programmatic notes:
# ---------------------
#   make_ode
#        Compiles an ODE for a given state
#
#   fetch_estimateable_outputs
#        To get list of outputs suitable for estimation
#
#   remap_namespace
#        Used to convert names used in different statements
#        For example renaming states and dynamic secondary parameters in the
#        ERROR block of NONMEM
#
#   fetch_padding   
#
#   apply_format
#
#   extract_conditional
#        
#
#   The following strings are used internally to
#   identify the different target types
#   
#      C
#      matlab
#      pw
#      bm
#      fortran
#      monolix
#      nonmem   
#      rproject
#
#
#    Adding a new target format
#    Modify the following functions
#          apply_format
#          extract_conditional
#          $nsmap->{STRING} = 'replacement';
#          remap_namespace(string, $nsmap);
#


use strict;
use warnings;
no warnings 'deprecated';
use Data::Dumper;
use File::Spec::Functions;

MAIN:
{
    
    # this is the data structure that contains all of the system information
    # as well as information about files used, reserved words, etc.
    my $cfg = &fetch_cfg();

    my (@lines, @lines_expanded, $line);
    my $file_handle;

    my $comment_trigger = 0;
    my $tmp_line;

    my $name;

    # Applying command-line arguments

    # First argument: system file:
    if(exists($ARGV[0])){
      $cfg->{files}->{system} = $ARGV[0]; }

    # Second argument: temporary folder
    if(exists($ARGV[1])){
      $cfg->{files}->{temp_directory} = $ARGV[1]; }

    # Third argument: template folder:
    if(exists($ARGV[2])){
      $cfg->{files}->{templates} = $ARGV[2]; }

    # Fourth argument: distribution type:
    if(exists($ARGV[3])){
      $cfg->{files}->{distribution} = $ARGV[3]; }

    # fifth argument: model prefix
    if(exists($ARGV[4])){
      $cfg->{files}->{model_prefix} = $ARGV[4]; }

    #
    # making sure the temporary directory is there
    #
    if (not(-d $cfg->{files}->{temp_directory})){
      mkdir($cfg->{files}->{temp_directory});}


    open(EQFH, $cfg->{files}->{system}) or die "unable to open $cfg->{files}->{system}";

    # reading in each line in system file
    { local $/=undef;  $file_handle=<EQFH>; }
    @lines=split /[\r\n]+/, $file_handle;
    close(EQFH);

    #
    # First Pass through system file to detect 
    # any sets that may be present
    #
    $comment_trigger = 0;
    foreach $line (@lines){
      #
      # preserving header comments
      #
      if(($comment_trigger eq 0) and 
         ($line =~ m/^#/) and 
      not($line =~ m/^\s*$/)){
        $tmp_line = $line."\n";
        $tmp_line =~ s/^#/SIMINT_COMMENT_STRING/;
        # storing the header comments in the 
        # following hash:
        $cfg->{comments} .= $tmp_line;
      }
      else{$comment_trigger = 1;}
      

      #stripping out comments
      $line =~ s/#.*$//;
      #stripping of trailing spaces
      $line =~ s#\s*$##;
      #stripping of leading
      $line =~ s#^\s*##;

      if($line =~ '<SET:\S+>\s*\S+'){
        $cfg  = &parse_set($cfg, $line);
      }
    }

    #
    # Second pass to expand lines with sets
    # to the different permutations that are
    # possible
    #
    @lines_expanded = ();
    foreach $line (@lines){
      # Check for summation and product
      # SIMINT_SET_SUM[]  SIMINT_SET_PRODUCT[]
      # if they exist then expand them to account for all entries
      $line = &apply_set_functions($cfg, $line);

      # checking to see if the line has any set information
      # basically looking for some string enclosed in curly braces:
      # {sometext}
      if($line =~ m#{.+}#){
        if(keys(%{$cfg->{sets}}) gt 0){
          # applying sets to the line
          push @lines_expanded, &apply_sets($cfg, $line);
          }
        else{
          &mywarn("Set found but no sets seem to have been defined"); }
      }
      else{
        push @lines_expanded, $line; }
    }

    #
    # processing system file
    # 
    foreach $line (@lines_expanded){
    #
    # Processing dynamic equations
    #
    # line contains compartment exchange information
    if($line =~ '<C>'){
      push @{$cfg->{equations}}, $line; 
      $cfg  = &parse_equation_C($cfg, $line);
     }
    # line contains source and sink informaiton
    if($line =~ '<S:\S+>'){
      push @{$cfg->{equations}}, $line; 
      $cfg  = &parse_equation_S($cfg, $line);
    }
    # line contains rate information
    if($line =~ '\s+=\S+=>'){
      push @{$cfg->{equations}}, $line; 
      $cfg  = &parse_equation_rate($cfg, $line);
    }

    # line contains forward/reverse rate information
    if($line =~ '<=\S+=>'){
      push @{$cfg->{equations}}, $line; 
      $cfg  = &parse_equation_fr_rate($cfg, $line);
    }

    # line contains odes
    if($line =~ '<ODE:\S+>\s*\S+'){
      push @{$cfg->{equations}}, $line; 
      $cfg  = &parse_ode($cfg, $line);
    }

    #
    # Parameters
    #
    if($line =~ '<P>'){
      $cfg  = &parse_parameter($cfg, $line, 'system'); }

    if($line =~ '<PSET'){
      $cfg  = &parse_parameter_set($cfg, $line, 'no'); }

    #
    # Static secondary parameters     
    #
    if($line =~ '<As>'){
      $cfg  = &parse_static_secondary_parameters($cfg, $line); }

    #
    # dynamic secondary parameters     
    #
    if($line =~ '<Ad>'){
      $cfg  = &parse_dynamic_secondary_parameters($cfg, $line); }

    #
    # initial conditions
    #
    if($line =~ '<I>'){
      $cfg  = &parse_initial_conditions($cfg, $line); }
    #
    # outputs
    #
    if($line =~ '<O>'){
      $cfg  = &parse_output($cfg, $line); }

    #
    # rate inputs
    #
    if($line =~ '<R:\S+>'){
      $cfg  = &parse_input_rate($cfg, $line); }

    #
    # covariates   
    #
    if($line =~ '<CV:\S+>'){
      $cfg  = &parse_input_covariate($cfg, $line); }

    if($line =~ '<CVSET:\S+:\S+>'){
      $cfg  = &parse_input_covariate($cfg, $line); }

    if($line =~ '<CVTYPE:\S+>'){
      $cfg  = &parse_input_covariate($cfg, $line); }

    #
    # iiv  inter-individual variability  
    #
    if($line =~ '<IIV:\S+>'    or $line =~ '<IIVCOR:\S+>' or
       $line =~ '<IIVSET:\S+>' or $line =~ '<IIVCORSET:\S+>'){
      $cfg  = &parse_interindivudal_varability($cfg, $line); }


    #
    # bolus inputs
    #
    if($line =~ '<B:\S+>'){
      $cfg  = &parse_bolus_inputs($cfg, $line); }

    #
    # variance information
    #
    # variance parameters are stored internally just like
    # parameters except they have a field that indicates 
    # they are different
    if($line =~ '<VP>'){
      $cfg  = &parse_parameter($cfg, $line, 'variance'); }


    if($line =~ '<VE:\S+>'){
      $cfg  = &parse_variance_equation($cfg, $line); }

    if($line =~ '<DV:\S+>'){
      $cfg  = &parse_error_equation($cfg, $line); }

   #if($line =~ '<VCVHEADER>'){
   #  $cfg  = &parse_vcv_header($cfg, $line); }
   #
   #if($line =~ '<VCVROW>'){
   #  $cfg  = &parse_vcv_row($cfg, $line); }


    #
    # if statement 
    #
    if($line =~ '<IF:\S+>'){
      $cfg  = &parse_if($cfg, $line); }



    #
    # Time scales
    #
    if($line =~ '<TS:\S+>\s+\S+'){
      $cfg  = &parse_time_scales($cfg, $line); }

    #
    # Options
    #
    if($line =~ '<OPT:\S+>\s+\S+.*'){
      $cfg  = &parse_option($cfg, $line); }

 #  # index
    if($line =~ '<INDEX:\S+>'){
      $cfg  = &parse_index($cfg, $line); }

 #  # amtify
    if($line =~ '<AMTIFY>'){
      $cfg  = &parse_amtify($cfg, $line); }

    #
    # Guide   
    #
    if($line =~ '<GUIDE>'){
      $cfg  = &parse_guide($cfg, $line); }


    #
    # Data files
    #
    if(($line =~ '<DATA:FILE:CSV>') or ($line =~ '<DATA:HEADER:.*>')){
      $cfg  = &parse_data_file($cfg, $line); }

    #
    # NONMEM specific options
    #
    if($line =~ '<NONMEM:'){
      $cfg  = &parse_nonmem_options($cfg, $line); }

  }

  # creating the file:
  # system_help_reserved_words.txt
  &dump_reserved_words($cfg);


  #checking the data file, column headers, etc
  &data_check($cfg);

  # checking for name clashes and other
  # possible mistakes. 
  &system_check($cfg);


  # re ordering states according 
  # to user specifications
  $cfg = &update_order($cfg);


  # dumping matlab
  &dump_matlab($cfg);

  # dumping rproject
  &dump_rproject($cfg);

  # creating the targets for each set of parameters
  foreach $name (keys(%{$cfg->{parameter_sets}})){
    &dump_adapt($cfg            , $name);
    &dump_berkeley_madonna($cfg , $name);
    &dump_potterswheel3($cfg    , $name);
    &dump_monolix($cfg          , $name);
    &dump_nonmem($cfg           , $name);
    &dump_mrgsolve($cfg         , $name);
  }

  exit 0;

}

sub fetch_cfg
{

my $cfg; 

  # default system file name
  $cfg->{files}->{system}                            = 'system.txt';

  # default template for building a system
  $cfg->{files}->{templates}                         =  &catfile('library', 'templates');

  # temporary directory to store target files and autsim scripts
  $cfg->{files}->{temp_directory}                    = 'transient';

  # indicates if ubiquity is running as a stand alone distribution  ("stand alone") or as 
  # part of a package ("package")
  $cfg->{files}->{distribution}                      = 'stand alone';

  # Prefix use in generated functions an files associated for the R workflow
  $cfg->{files}->{model_prefix}                      = 'ubiquity_model';

  # C files
  $cfg->{files}->{initialize}                        = 'auto_initial_sizes.h';
  $cfg->{files}->{common_block}                      = 'auto_common_block.h';
  $cfg->{files}->{remap_odes}                        = 'auto_remap_odes.h';
  $cfg->{files}->{odes}                              = 'auto_odes.h';
  $cfg->{files}->{outputs}                           = 'auto_outputs.h';

  # matlab files
  $cfg->{files}->{fetch_system_information}          = 'auto_fetch_system_information.m';
  $cfg->{files}->{map_simulation_output}             = 'auto_map_simulation_output.m';
  $cfg->{files}->{simulation_driver}                 = 'auto_simulation_driver.m';
  $cfg->{files}->{analysis_estimation}               = 'auto_analysis_estimation.m';

  # used to run the ode in m format
  $cfg->{files}->{sim_m}                             = 'auto_sim.m';
  $cfg->{files}->{odes_m}                            = 'auto_odes.m';

  # potterswheel output
  $cfg->{files}->{potterswheel2}                     = 'target_pw_2.m';

  # potterswheel output
  $cfg->{files}->{potterswheel3}                     = 'target_pw_3';

  # rproject
  $cfg->{files}->{rproject}->{components}           = 'auto_rcomponents.R';
  $cfg->{files}->{rproject}->{simulation_driver}    = 'auto_simulation_driver.R';
  $cfg->{files}->{rproject}->{compiled}             = 'r_ode_model.c';
  $cfg->{files}->{rproject}->{analysis_estimation}  = 'auto_analysis_estimation.R';

  # berkeley_madonna output
  $cfg->{files}->{berkeley_madonna}                  = 'target_berkeley_madonna';


  # adapt 5 output
  # This is the prefix for the
  # different files generated 
  # for adapt and extensions 
  # will be added as appropriate
  # (.for, .prm, etc)
  $cfg->{files}->{adapt}                             = 'target_adapt_5';

  $cfg->{files}->{mrgsolve}                          = 'target_mrgsolve';
  $cfg->{files}->{nonmem}                            = 'target_nonmem';
  $cfg->{files}->{monolix}                           = 'target_monolix';

  # berkeley_madonna output
  $cfg->{files}->{reserved_words}                    = 'system_help_reserved_words.txt';

  # hash elements to hold system information
  @{$cfg->{equations}}                                  = ();
  $cfg->{parameters}                                 = {};
  @{$cfg->{parameters_index}}                           = ();
  $cfg->{parameter_sets}                             = {};
  @{$cfg->{parameter_sets_index}}                       = ();
  # making sure there is an entry for the 'default' parameter set.
  $cfg = &initialize_parameter_set($cfg, 'default');
  $cfg->{parameter_sets}->{default}->{name}          = 'default';

  # these break the parameters down between system 
  # and variance parameters
  @{$cfg->{parameters_system_index}}                    = ();
  @{$cfg->{parameters_variance_index}}                  = ();

  $cfg->{static_secondary_parameters}                = {};
  @{$cfg->{static_secondary_parameters_index}}          = ();
  $cfg->{dynamic_secondary_parameters}               = {};
  @{$cfg->{dynamic_secondary_parameters_index}}      = ();
  $cfg->{initial_conditions}                         = {};
  $cfg->{bolus_inputs}                               = {};
  $cfg->{input_rates}                                = {};
  @{$cfg->{input_rates_index}}                       = ();
  $cfg->{covariates}                                 = {};
  @{$cfg->{covariates_index}}                        = ();
  $cfg->{iiv}                                        = {};
  $cfg->{iiv_index}                                  = {};
  $cfg->{species}                                    = {};
  @{$cfg->{species_index}}                           = ();
  $cfg->{outputs}                                    = {};
  @{$cfg->{outputs_index}}                           = ();
  $cfg->{guides}                                     = {};
  $cfg->{options}                                    = {};
  $cfg->{options}->{output_times}                    = 'SIMINT_SEQ[0][10][.1]';
  $cfg->{options}->{nonmem}->{input}->{drop}         = {};
  $cfg->{options}->{nonmem}->{input}->{rename}       = {};
  $cfg->{options}->{nonmem}->{data}                  = '';
  $cfg->{options}->{amtify}->{cmt_to_amt}            = {};
  $cfg->{options}->{amtify}->{cmt_to_rel}            = {};
  $cfg->{index}->{STATE}->{byname}                   = {};
  $cfg->{index}->{STATE}->{byvalue}                  = {};
  $cfg->{current_section}                            = '';
  @{$cfg->{times_scales_index}}                      = ();
  $cfg->{times_scales}                               = {};

  $cfg->{variance}->{equations}                      = {};
  $cfg->{error}->{equations}                         = {};

  $cfg->{sets}                                       = {};
  
  $cfg->{if_conditional}                             = {};

  $cfg->{comments}                                   = "";

  $cfg->{data}->{file}                               = '';
  @{$cfg->{data}->{headers}->{values}}               = ();
  $cfg->{data}->{headers}->{mode}                    = ''; #manual or automoatic
                                                     
  # formatting parameters
  $cfg->{term_length}                                = 20;
  $cfg->{inputs_length}                              = 10;
  $cfg->{species_length}                             = 12;
  $cfg->{outputs_length}                             = 12;
  $cfg->{time_scales_length}                         = 12;
  $cfg->{parameters_length}                          = 12;
  $cfg->{parameter_values_length}                    = 10; 
  $cfg->{parameter_text_length}                      = 15; 



  #
  # data structure containing reserved words and their
  # associated programs
  # 
  # 
  #  insensitive = full string; case insensitive
  #  exact       = full string; case sensitive 
  #  start       = strings begin with this; case sensitive
  # 
  # 
  $cfg->{reserved} = {
        'ADAPT 5'          => {
                                IC         =>  'insensitive' ,
                                P          =>  'insensitive' ,
                                PS         =>  'insensitive' ,
                                T          =>  'insensitive' ,
                                Y          =>  'insensitive' ,
                                V          =>  'insensitive' ,
                                XP         =>  'insensitive' ,
                                X          =>  'insensitive' 
                              },
        'Matlab'           => {
                                x          =>  'exact' ,
                                dx         =>  'exact' ,
                                y          =>  'exact' ,
                                tid        =>  'exact' ,
                                S          =>  'exact' ,
                                SIMINT_    =>  'start' 
                              },
        'Berkeley Madonna' => {
                                STARTTIME  => 'insensitive' ,
                                STOPTIME   => 'insensitive' ,
                                DT         => 'insensitive' ,
                                DTMIN      => 'insensitive' ,
                                DTMAX      => 'insensitive' ,
                                TOLERANCE  => 'insensitive' ,
                                DTOUT      => 'insensitive' ,
                                ROOTTOL    => 'insensitive' ,
                                TIME       => 'insensitive' 
                              },
        'Monolix'          => {
                                pop_       => 'start'       ,
                              },
        'Nonmem'           => {
                                'S\d+'     => 'start'       ,
                              },
        'R-project'        => {
                                'NA'            => 'exact',
                                'if'            => 'exact',
                                'else'          => 'exact',
                                'repeat'        => 'exact',
                                'while'         => 'exact',
                                'funciton'      => 'exact',
                                'for'           => 'exact',
                                'next'          => 'exact',
                                'break'         => 'exact',
                                'TRUE'          => 'exact',
                                'FALSE'         => 'exact',
                                'Inf'           => 'exact',
                                'NA_integer'    => 'start',
                                'NA_real'       => 'start',
                                'NA_complex'    => 'start',
                                'NA_character'  => 'start'
                              },
        'C'                => {
                               auto        => 'exact' ,
                               else        => 'exact' ,
                               long        => 'exact' ,
                               switch      => 'exact' ,
                               break       => 'exact' ,
                               enum        => 'exact' ,
                               register    => 'exact' ,
                               typedef     => 'exact' ,
                               case        => 'exact' ,
                               extern      => 'exact' ,
                               return      => 'exact' ,
                               union       => 'exact' ,
                               char        => 'exact' ,
                               float       => 'exact' ,
                               short       => 'exact' ,
                               unsigned    => 'exact' ,
                               const       => 'exact' ,
                               for         => 'exact' ,
                               signed      => 'exact' ,
                               void        => 'exact' ,
                               continue    => 'exact' ,
                               goto        => 'exact' ,
                               sizeof      => 'exact' ,
                               volatile    => 'exact' ,
                               default     => 'exact' ,
                               if          => 'exact' ,
                               static      => 'exact' ,
                               while       => 'exact' ,
                               do          => 'exact' ,
                               int         => 'exact' ,
                               struct      => 'exact' ,
                               PI          => 'exact' ,
                               PI_2        => 'exact' ,
                               PI_4        => 'exact' ,
                               SQRTPI      => 'exact' ,
                               SQRT2       => 'exact' ,
                               SQRT1_2     => 'exact' ,
                               E           => 'exact' ,
                               LOG2E       => 'exact' ,
                               LOG10E      => 'exact' ,
                               _Packed     => 'exact' ,
                               double      => 'exact' 
                              }
  };


return($cfg);

}

sub dump_reserved_words
{
  my ($cfg) = @_;

  my $output_file = '';
  my $program;
  my $word   ;

  my $lengths;

  $lengths->{word}    = 15;
  $lengths->{program} = 30;
  $lengths->{notes}   = 45;


   $output_file .= 'keyword'.&fetch_padding('keyword', $lengths->{word});
   $output_file .= 'program'.&fetch_padding('program', $lengths->{program});
   $output_file .= 'notes';
   $output_file .= "\n";
   $output_file .= '-'x$lengths->{word};
   $output_file .= '-'x$lengths->{program};
   $output_file .= '-'x$lengths->{notes};
   $output_file .= "\n";

  foreach $program (keys(%{$cfg->{reserved}})){
    foreach $word   (keys(%{$cfg->{reserved}->{$program}})){
       $output_file .= $word.&fetch_padding($word,    $lengths->{word});
       $output_file .= $program.&fetch_padding($program, $lengths->{program});
  
       if($cfg->{reserved}->{$program}->{$word} eq 'start'){
         $output_file .= 'Names beginning with this text'; }
       else{
         $output_file .= 'Exact match'; }
       $output_file .= "\n";
    }
  }

  $output_file .= '-'x$lengths->{word};
  $output_file .= '-'x$lengths->{program};
  $output_file .= '-'x$lengths->{notes};
  $output_file .= "\n";

  open(FH, '>', &catfile( $cfg->{files}->{temp_directory}, $cfg->{files}->{reserved_words}));
  print FH $output_file;
  close(FH);

}

sub dump_rproject
{
  my ($cfg) = @_;

  my $counter;
  my $parameter; 
  my $set;       
  my $pname      = '';

  my $iname      = '';
  my $iname2     = '';
  my $counter2;

  my $counter_ode_outputs = 1;

  # used to keep track of forcing function counter in C
  my $counter_ff_C = 0;
  my $namespace_map; 

  my $state; 
  my $sname      = '';

  my $rate;  
  my $rname      = '';

  my $covariate; 
  my $cname      = '';

  my $option;

  my $output;
  my $oname      = '';
  my $field      = '';
  my $field_times= '';
  my $field_state= '';
  my @fields     = '';
  my $field_value= '';


  my $indent     = '   ';
  my $mc;
  #my $template_components = &fetch_rproject_components_template();
  my $template_components = &fetch_template_file($cfg, 'r_components.R');
  $mc->{COMMENTS}                     = '';
  $mc->{FETCH_SYS_PARAMS}             = '';
  $mc->{FETCH_SYS_SSP}                = '';
  $mc->{FETCH_SYS_DSP}                = '';
  $mc->{FETCH_SYS_INDICES}            = '';
  $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} = '';
  $mc->{FETCH_SYS_IC}                 = '';
  $mc->{FETCH_SYS_IIV}                = '';
  $mc->{FETCH_SYS_VE}                 = '';
  $mc->{FETCH_SYS_PSETS}              = '';
  $mc->{FETCH_SYS_TS}                 = '';
  $mc->{FETCH_SYS_DATA}               = '';
  $mc->{FETCH_SYS_MISC}               = '';
  $mc->{FETCH_SYS_BOLUS}              = '';
  $mc->{FETCH_SYS_INFUSIONS}          = '';
  $mc->{FETCH_SYS_COVARIATES}         = '';
  $mc->{COVARIATES_IC}                = '';
  $mc->{COVARIATES}                   = '';
  $mc->{INFUSION_RATES}               = '';
  $mc->{SS_PARAM}                     = '';
  $mc->{DS_PARAM}                     = '';
  $mc->{ODES}                         = '';
  $mc->{ODES_REMAP}                   = '';
  $mc->{OUTPUTS}                      = '';
  $mc->{SELECT_PARAMS}                = '';
  $mc->{STATES}                       = '';
  $mc->{SYSTEM_PARAM}                 = '';
  $mc->{TIME_SCALES}                  = '';
  $mc->{SYSTEM_FILE}                  = $cfg->{files}->{system};
  $mc->{DISTRIBUTION}                 = $cfg->{files}->{distribution};
  $mc->{TEMP_DIRECTORY}               = $cfg->{files}->{temp_directory};
  $mc->{MODEL_PREFIX}                 = $cfg->{files}->{model_prefix};

  my $md;
  #my $template_driver     = &fetch_rproject_simulation_driver_template();
  my $template_driver     = &fetch_template_file($cfg, 'r_simulation_driver.R');
  $md->{PSETS}                 = '';
  $md->{BOLUS}                 = '';
  $md->{INFUSION_RATES}        = '';
  $md->{COVARIATES}            = '';
  $md->{OUTPUT_TIMES}          = '';
  $md->{SYSTEM_FILE}           = $cfg->{files}->{system};


  my $ma;
  my $template_analysis        = &fetch_template_file($cfg, 'r_analysis_estimation.R ');
  $ma->{PSETS}                 = '';
  $ma->{BOLUS}                 = '';
  $ma->{INFUSION_RATES}        = '';
  $ma->{COVARIATES}            = '';
  $ma->{OUTPUT_TIMES}          = '';
  $ma->{SYSTEM_FILE}           = $cfg->{files}->{system};

  if($cfg->{files}->{distribution} eq "stand alone"){
    $md->{DIST_HEADER}           = 'source(file.path("library", "r_general", "ubiquity.R"))'."\n";
    $ma->{DIST_HEADER}           = 'source(file.path("library", "r_general", "ubiquity.R"))'."\n";

  } else {
    $md->{DIST_HEADER}           = '# Uncomment the following to use with "stand alone" distribution'."\n";
    $ma->{DIST_HEADER}           = '# Uncomment the following to use with "stand alone" distribution'."\n";
    $md->{DIST_HEADER}          .= '# source(file.path("library", "r_general", "ubiquity.R"))'."\n";
    $ma->{DIST_HEADER}          .= '# source(file.path("library", "r_general", "ubiquity.R"))'."\n";
  }

  my $mo;
  #my $template_compiled   = &fetch_rproject_compiled_template();
  my $template_compiled   = &fetch_template_file($cfg, 'r_odes.c');
  $mo->{SYSTEM_PARAM}          = '';
  $mo->{VARIABLE_INIT}         = '';
  $mo->{STATES}                = '';
  $mo->{ODES}                  = '';
  $mo->{ODES_REMAP}            = '';
  $mo->{TIME_SCALES}           = '';
  $mo->{SS_PARAM}              = '';
  $mo->{DS_PARAM}              = '';
  $mo->{OUTPUTS}               = '';
  $mo->{OUTPUTS_REMAP}         = '';
  $mo->{NPARAMS}               = scalar(@{$cfg->{parameters_index}});
  $mo->{FORCEFUNC}             = '';
  $mo->{FORCE_PARAM}           = '';
  $mo->{FORCEDECLARE}          = '';


#
# defining aspects of forcing functions in C
#
# the total number of forcing functions is 1 for each infusion rate and two for
# the covariates (1 for the initial condition and 1 for the initial value of the
# covariate in case it's used  as a secondary parameter
if((scalar(@{$cfg->{covariates_index}}) + 
    scalar(@{$cfg->{input_rates_index}})) gt 0) {
     
$mo->{FORCEFUNC} .= 'void initforcs(void (* odeforcs)(int *, double *))
{
    int N='.(2*scalar(@{$cfg->{covariates_index}}) + scalar(@{$cfg->{input_rates_index}})).';
    odeforcs(&N, forc);
}';
     
$mo->{FORCEDECLARE}   = "\nstatic double forc[".(2*scalar(@{$cfg->{covariates_index}}) + scalar(@{$cfg->{input_rates_index}}))."];";
}

#
# First we create the parameters field with the parameter 'matrix' first
#
$mc->{FETCH_SYS_PARAMS} = '# defining the matrix of parameter information
cfg$parameters$matrix = data.frame('."\n";

$mc->{FETCH_SYS_PARAMS} .= $indent."name".&fetch_padding("name", $cfg->{parameters_length})." = c(";
# creating the parameter names
  foreach $parameter (@{$cfg->{parameters_index}}){
    $pname = $parameter;
    if($pname  eq $cfg->{parameters_index}[-1]){
      $pname = '"'.$pname.'"'; }
    else{
      $pname = '"'.$pname.'",';
      $pname = $pname.&fetch_padding($pname, $cfg->{parameters_length}); }
    $mc->{FETCH_SYS_PARAMS} .= $pname;

  }
$mc->{FETCH_SYS_PARAMS} .= ")";

# now looping through each field
@fields = qw(value lower_bound upper_bound ptype editable type units);
foreach $field (@fields){
$mc->{FETCH_SYS_PARAMS} .= ",\n";
$mc->{FETCH_SYS_PARAMS} .= $indent.$field.&fetch_padding($field, $cfg->{parameters_length})." = c(";
  foreach $parameter (@{$cfg->{parameters_index}}){
    $pname = $parameter;
    $field_value = $cfg->{parameters}->{$pname}->{$field};
    # converting eps, -eps, -inf and inf into
    # r-specfic  language
    if(($field eq "lower_bound") or ($field eq "upper_bound")){
     if($field_value eq  "eps"){ $field_value = '.Machine$double.eps';}      #  epsilon   
     if($field_value eq "-eps"){ $field_value = '.Machine$double.neg.eps';}  # -epsilon   
     if($field_value eq  "inf"){ $field_value = '.Machine$double.xmax';}     #  infinity 
     if($field_value eq "-inf"){ $field_value = '.Machine$double.xmin';}     # -infinity 
    }
    # quoting the text fields
    if(($field eq "editable" ) or 
       ($field eq "ptype"    ) or 
       ($field eq "type"     ) or 
       ($field eq "units"    )){
       $field_value = '"'.$field_value.'"';
    }
    if($pname  ne $cfg->{parameters_index}[-1]){
      $field_value = $field_value.",";
      $field_value = $field_value.&fetch_padding($field_value, $cfg->{parameters_length}); }
      $mc->{FETCH_SYS_PARAMS} .= $field_value;
  }
$mc->{FETCH_SYS_PARAMS} .= ")";
}
$mc->{FETCH_SYS_PARAMS} .= ")\n";

#
# Processing components
#
$mc->{FETCH_SYS_INDICES} .= "# defining indices\n";
$mc->{FETCH_SYS_INDICES} .= "# parameters\n";

# States    
# These are the components that are state dependent like ODEs, initial
# conditions, etc.
$mc->{FETCH_SYS_INDICES} .= "# states   \n";
$mo->{OUTPUTS_REMAP}      .= "/* States */\n";
$counter = 1;
foreach $state     (@{$cfg->{species_index}}){
  $sname = $state;

  # collecting all of the outputs to include with the simulation outputs
  #$mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."$sname".&fetch_padding($sname, $cfg->{species_length})." = $counter_ode_outputs \n";
  # $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = $sname;\n";

  # R-script level
  $mc->{FETCH_SYS_INDICES} .= 'cfg$options$mi$states$'."$sname".&fetch_padding($sname, $cfg->{species_length})." = $counter \n";
  $mc->{STATES}         .= $sname.&fetch_padding($sname, $cfg->{species_length})." = SIMINT_x[$counter];\n";
  $mc->{ODES}           .= "SIMINT_d$sname".&fetch_padding("SIMINT_d$sname", $cfg->{species_length})." = ".&make_ode($cfg, $sname, 'rproject').";\n";
  $mc->{ODES_REMAP}     .= $indent."SIMINT_d$sname";
  $mc->{STATE_ICS_REMAP}.= $indent.$sname.&fetch_padding("$sname", $cfg->{species_length})." = c(SIMINT_$sname"."_IC)";

  if(defined($cfg->{initial_conditions}->{$sname})){
    $mc->{FETCH_SYS_IC}      .= 'cfg$options$initial_conditions$'."$sname".&fetch_padding($sname, $cfg->{species_length})." = '";
    $mc->{FETCH_SYS_IC}      .= &apply_format($cfg, $cfg->{initial_conditions}->{$sname}, 'rproject')."' \n";
  }
  

  #commas until the last state
  if($counter < scalar( (@{$cfg->{species_index}}))){
    $mc->{ODES_REMAP} .= ", \n"; 
    $mc->{STATE_ICS_REMAP} .= ", \n"; }

  # Compiled code level
  if($counter eq 1){
    $mo->{VARIABLE_INIT} .= "/* States and ODEs */\n";
  }
  $mo->{VARIABLE_INIT} .= "double $sname".&fetch_padding($sname, $cfg->{species_length})." = 0.0;\n";
  $mo->{VARIABLE_INIT} .= "double SIMINT_d$sname".&fetch_padding("SIMINT_d$sname", $cfg->{species_length})." = 0.0;\n";

  $mo->{STATES}     .= "$sname".&fetch_padding($sname, $cfg->{species_length})." = y[".($counter-1)."];\n";
  $mo->{ODES}       .= "SIMINT_d$sname".&fetch_padding($sname, $cfg->{species_length})." = ";
  $mo->{ODES}       .= &make_ode($cfg, $sname, 'C').";\n";
  $mo->{ODES_REMAP} .= "ydot[".($counter-1)."] = SIMINT_d$sname;\n";

  
  $counter             = 1 + $counter;
  #$counter_ode_outputs = 1 + $counter_ode_outputs;

}



# Outputs   
$mc->{FETCH_SYS_INDICES} .= "# outputs  \n";
$mo->{OUTPUTS_REMAP}      .= "/* Model Outputs */\n";
$counter = 1;

foreach $output    (@{$cfg->{outputs_index}}){
  $sname = $output;

  # output indices
  $mc->{FETCH_SYS_INDICES} .= 'cfg$options$mi$outputs$'."$sname".&fetch_padding($sname, $cfg->{outputs_length})." = $counter \n";

  # output definitions used in mapping simulation output
  $mc->{OUTPUTS}           .= $sname.&fetch_padding($sname, $cfg->{outputs_length})." = ";
  $mc->{OUTPUTS}           .= &apply_format($cfg, $cfg->{outputs}->{$sname}, 'rproject')."\n";

  # Compiled code level
  if($mo->{OUTPUTS} eq ""){
    $mo->{VARIABLE_INIT} .= "/* Outputs*/\n";
  }
  $mo->{VARIABLE_INIT}     .= "double $sname".&fetch_padding($sname, $cfg->{outputs_length})." = 0.0;\n";
  $mo->{OUTPUTS}           .= $sname.&fetch_padding($sname, $cfg->{outputs_length})." = ";
  $mo->{OUTPUTS}           .= &apply_format($cfg, $cfg->{outputs}->{$sname}, 'C').";\n";

  # collecting all of the outputs to include with the simulation outputs
  $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."$sname".&fetch_padding($sname, $cfg->{species_length})." = $counter_ode_outputs \n";
  $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = $sname;\n";

  $counter = 1+$counter;
  $counter_ode_outputs = 1 + $counter_ode_outputs;

}


# Parameters
$mo->{OUTPUTS_REMAP}      .= "/* System Parameters */\n";
$counter = 1;
foreach $parameter (@{$cfg->{parameters_index}}){
  $pname = $parameter;
  # defining the indices of the parameters
  $mc->{FETCH_SYS_INDICES} .= 'cfg$options$mi$parameters$'."$pname".&fetch_padding($pname, $cfg->{parameters_length})." = $counter \n";
  $mc->{SYSTEM_PARAM}  .= $pname.&fetch_padding($pname, $cfg->{parameters_length}).' = SIMINT_p$'.$pname."\n";
  $mc->{SELECT_PARAMS} .= "  ".$pname.&fetch_padding($pname, $cfg->{parameters_length}).' = c(cfg$parameters$matrix$value['.$counter.'])';
  if($counter < scalar(@{$cfg->{parameters_index}})){
    $mc->{SELECT_PARAMS} .= ",\n"; }

  # collecting all of the outputs to include with the simulation outputs
  $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."$parameter".&fetch_padding($parameter, $cfg->{species_length})." = $counter_ode_outputs \n";
  $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = $parameter;\n";
  $counter_ode_outputs = 1 + $counter_ode_outputs;

  # Compiled C Code
  $mo->{SYSTEM_PARAM} .= "#define $pname".&fetch_padding($pname, $cfg->{parameters_length})." parms[".($counter-1)."]\n";
  $counter = 1+$counter;

}


# IIV

my $set_id ;


if(keys(%{$cfg->{iiv}})){
  $mc->{FETCH_SYS_INDICES} .= "# iiv \n";
  foreach $set_id (keys %{$cfg->{iiv_index}}){
    $counter = 1;
    # iiv indices
    foreach $parameter    (@{$cfg->{iiv_index}->{$set_id}}){
      $iname = $parameter;
      $mc->{FETCH_SYS_INDICES} .= 'cfg$options$mi$iiv_sets$'.$set_id.'$'."$iname".&fetch_padding($iname, $cfg->{outputs_length})." = $counter \n";
      $counter = 1+$counter;
    }
    
    # listing the parameters the IIVs apply to
    $counter = 1;
    foreach $parameter    (@{$cfg->{iiv_index}->{$set_id}}){
      $iname = $parameter;
      $mc->{FETCH_SYS_IIV}     .= 'cfg$iiv$sets$'.$set_id.'$iivs$'."$iname".'$parameters'.&fetch_padding($iname, $cfg->{parameters_length}).' =c("'.join('", "', @{$cfg->{iiv}->{$set_id}->{iivs}->{$iname}->{parameters}}).'"'.")\n";
      $counter = 1+$counter;
    }
    # defining the parameter specific information (distributions and reverse
    # mapping to the IIV terms);
    foreach $iname     (keys(%{$cfg->{iiv}->{$set_id}->{parameters}})){
      $mc->{FETCH_SYS_IIV}     .=    'cfg$iiv$sets$'.$set_id.'$parameters$'.$iname.'$iiv_name    '.&fetch_padding($iname, $cfg->{parameters_length})." = '".$cfg->{iiv}->{$set_id}->{parameters}->{$iname}->{iiv_name}."'\n";
      $mc->{FETCH_SYS_IIV}     .=    'cfg$iiv$sets$'.$set_id.'$parameters$'.$iname.'$distribution'.&fetch_padding($iname, $cfg->{parameters_length})." = '".$cfg->{iiv}->{$set_id}->{parameters}->{$iname}->{distribution}."'\n";
      $mc->{FETCH_SYS_IIV}     .=    'cfg$iiv$sets$'.$set_id.'$parameters$'.$iname.'$equation    '.&fetch_padding($iname, $cfg->{parameters_length})." = '".&make_iiv($cfg, $iname, 'rproject', $set_id)."'\n";
    }
    $mc->{FETCH_SYS_IIV}     .= 'cfg$iiv$sets$'.$set_id.'$values = matrix(0,'.scalar(@{$cfg->{iiv_index}->{$set_id}}).",".scalar(@{$cfg->{iiv_index}->{$set_id}}).")\n";
    $counter = 1;
    foreach $iname     (@{$cfg->{iiv_index}->{$set_id}}){
      $counter2 = 1;
      foreach $iname2    (@{$cfg->{iiv_index}->{$set_id}}){
        if(defined($cfg->{iiv}->{$set_id}->{vcv}->{$iname}->{$iname2})){
          $mc->{FETCH_SYS_IIV}     .=  'cfg$iiv$sets$'.$set_id.'$values['."$counter, $counter2] =  $cfg->{iiv}->{$set_id}->{vcv}->{$iname}->{$iname2}\n";
        }
        $counter2 = $counter2+1;
      }
    $counter = $counter+1;
    }

  }
  $mc->{FETCH_SYS_IIV}     .= "# Defaulting to the default set\n";
  $mc->{FETCH_SYS_IIV}     .= 'cfg$iiv$current_set = "default"'."\n";
  $mc->{FETCH_SYS_IIV}     .= 'cfg$iiv$iivs        = cfg$iiv$sets$default$iivs'."\n";
  $mc->{FETCH_SYS_IIV}     .= 'cfg$iiv$parameters  = cfg$iiv$sets$default$parameters'."\n";
  $mc->{FETCH_SYS_IIV}     .= 'cfg$iiv$values      = cfg$iiv$sets$default$values    '."\n";
  $mc->{FETCH_SYS_INDICES} .= "# Defaulting to the default set\n";
  $mc->{FETCH_SYS_INDICES} .= 'cfg$options$mi$iiv = cfg$options$mi$iiv_sets$default'."\n";

  # adding variance equations
  if ((keys(%{$cfg->{variance}->{equations}}))){
     foreach $iname  (keys(%{$cfg->{variance}->{equations}})){
        $mc->{FETCH_SYS_VE} .=  "cfg\$ve\$$iname = '".&apply_format($cfg, $cfg->{variance}->{equations}->{$iname}, 'rproject')."'\n";
     }
    }
  else{
    $mc->{FETCH_SYS_VE} .= "# No variance equations defined\n";
    $mc->{FETCH_SYS_VE} .= "# See: <VE>\n";
    $mc->{FETCH_SYS_VE} .= "cfg\$ve  = c()\n";}


}



# infusion rates
if((@{$cfg->{input_rates_index}})){
  $mo->{OUTPUTS_REMAP}      .= "/* Infusion Rates */\n";
  # simulation driver
  $md->{INFUSION_RATES} .= "# To overwrite the default infusion \n";
  $md->{INFUSION_RATES} .= "# inputs uncomment the lines below\n";


  $mc->{FETCH_SYS_INFUSIONS} .= "# Infusion rates\n";
  $mc->{FETCH_SYS_INFUSIONS} .= 'cfg$options$inputs$infusion_rate_names = '."c('".join("','", @{$cfg->{input_rates_index}})."')\n";
  foreach $rate  (@{$cfg->{input_rates_index}}){
    $rname = $rate;
    
    $mc->{FETCH_SYS_INFUSIONS} .= 'cfg$options$inputs$infusion_rates$'.$rname.'$times$values  = '."c(".join(', ', &extract_elements($cfg->{input_rates}->{$rname}->{times}->{values})).")\n";
    $mc->{FETCH_SYS_INFUSIONS} .= 'cfg$options$inputs$infusion_rates$'.$rname.'$times$scale   = '."'".$cfg->{input_rates}->{$rname}->{times}->{scale}."'\n";
    $mc->{FETCH_SYS_INFUSIONS} .= 'cfg$options$inputs$infusion_rates$'.$rname.'$times$units   = '."'".$cfg->{input_rates}->{$rname}->{times}->{units}."'\n";


    $mc->{FETCH_SYS_INFUSIONS} .= 'cfg$options$inputs$infusion_rates$'.$rname.'$levels$values = '."c(".join(', ', &extract_elements($cfg->{input_rates}->{$rname}->{levels}->{values})).")\n";
    $mc->{FETCH_SYS_INFUSIONS} .= 'cfg$options$inputs$infusion_rates$'.$rname.'$levels$scale  = '."'".$cfg->{input_rates}->{$rname}->{levels}->{scale}."'\n";
    $mc->{FETCH_SYS_INFUSIONS} .= 'cfg$options$inputs$infusion_rates$'.$rname.'$levels$units  = '."'".$cfg->{input_rates}->{$rname}->{levels}->{units}."'\n\n";


    # simulation driver
    $md->{INFUSION_RATES} .= "# cfg = system_set_rate(cfg, rate   = \"$rname\", \n";
    $md->{INFUSION_RATES} .= "#                            times  = c(".join(', ', &extract_elements($cfg->{input_rates}->{$rname}->{times}->{values}))."),  # $cfg->{input_rates}->{$rname}->{times}->{units}  \n";
    $md->{INFUSION_RATES} .= "#                            levels = c(".join(', ', &extract_elements($cfg->{input_rates}->{$rname}->{levels}->{values})).")) # $cfg->{input_rates}->{$rname}->{levels}->{units} \n";

    # analysis estimation
    $ma->{INFUSION_RATES} .= 'cohort$inputs$infusion_rates$'.$rname.'$TIME'.&fetch_padding($rname,10).'= c()'." # $cfg->{input_rates}->{$rname}->{times}->{units}  \n";
    $ma->{INFUSION_RATES} .= 'cohort$inputs$infusion_rates$'.$rname.'$AMT'.&fetch_padding($rname,11).'= c()'." # $cfg->{input_rates}->{$rname}->{levels}->{units} \n";
    

    #JMH this seems aberrant 2015.04.08
    # $mc->{INFUSION_RATES} .= $rname.&fetch_padding($rname, $cfg->{inputs_length})." = 0.0\n";

    # collecting all of the outputs to include with the simulation outputs
    $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."$rname".&fetch_padding($rname, $cfg->{species_length})." = $counter_ode_outputs \n";
    $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = $rname;\n";
    $counter_ode_outputs = 1 + $counter_ode_outputs;

    # c target
    $mo->{FORCE_PARAM} .= "#define $rname".&fetch_padding($rname, $cfg->{inputs_length})." forc[$counter_ff_C]\n";
    $counter_ff_C = $counter_ff_C +1;
  }
}

$mo->{OUTPUTS_REMAP}      .= "/* Covariates */\n";

# covariates    
if((@{$cfg->{covariates_index}})){
  $md->{COVARIATES} .= "# Covariates are set using the system_set_covariate statement.\n";
  $md->{COVARIATES} .= "# The default values are listed here, and they may be \n";
  $md->{COVARIATES} .= "# different than the current values. Uncomment to change them.\n";
  $mc->{FETCH_SYS_COVARIATES} .= "#Covariates \n";
  foreach $covariate  (@{$cfg->{covariates_index}}){
    $cname = $covariate; 

    # simulation driver
    $md->{COVARIATES} .= "# cfg = system_set_covariate(cfg, \"$cname\",";
    $md->{COVARIATES} .=  &fetch_padding(" $cname ,", 30)."# $cfg->{covariates}->{$cname}->{cv_type}\n";
    $md->{COVARIATES} .= "#                                 times  = c(".join(', ', &extract_elements($cfg->{covariates}->{$cname}->{parameter_sets}->{default}->{times}))."),";
    $md->{COVARIATES} .=  &fetch_padding("times  = c(".join(', ', &extract_elements($cfg->{covariates}->{$cname}->{parameter_sets}->{default}->{times}))."),", 30)."# $cfg->{covariates}->{$cname}->{times}->{units} \n";
    $md->{COVARIATES} .= "#                                 values = c(".join(', ', &extract_elements($cfg->{covariates}->{$cname}->{parameter_sets}->{default}->{values}))."))";
    $md->{COVARIATES} .=  &fetch_padding("values = c(".join(', ', &extract_elements($cfg->{covariates}->{$cname}->{parameter_sets}->{default}->{values}))."))", 30)."# $cfg->{covariates}->{$cname}->{values}->{units} \n";
    
    # analysis estimation
    $ma->{COVARIATES}     .= 'cohort$inputs$covariates$'.$cname.'$TIME'.&fetch_padding($cname,14).'= c()'." # $cfg->{covariates}->{$cname}->{times}->{units} \n";
    $ma->{COVARIATES}     .= 'cohort$inputs$covariates$'.$cname.'$AMT'.&fetch_padding($cname,15).'= c()'." # $cfg->{covariates}->{$cname}->{values}->{units} \n";

    # rcomponents
    $mc->{FETCH_SYS_COVARIATES} .= 'cfg$options$inputs$covariates$'.$cname.'$cv_type       = '."'".$cfg->{covariates}->{$cname}->{cv_type}."'\n";
    $mc->{FETCH_SYS_COVARIATES} .= 'cfg$options$inputs$covariates$'.$cname.'$times$units   = '."'".$cfg->{covariates}->{$cname}->{times}->{units}."'\n";
    $mc->{FETCH_SYS_COVARIATES} .= 'cfg$options$inputs$covariates$'.$cname.'$values$units  = '."'".$cfg->{covariates}->{$cname}->{values}->{units}."'\n";

    $mc->{COVARIATES_IC} .= $cname.' = SIMINT_cfg$options$inputs$covariates$'.$cname.'$values$values[1]'."\n";


    # c target
    $mo->{FORCE_PARAM} .= "#define $cname".&fetch_padding($cname, $cfg->{inputs_length})." forc[$counter_ff_C]\n";
    $counter_ff_C = $counter_ff_C +1;
    $mo->{FORCE_PARAM} .= "#define SIMINT_CVIC_$cname".&fetch_padding("SIMINT_CVIC_$cname", $cfg->{inputs_length})." forc[$counter_ff_C]\n";
    $counter_ff_C = $counter_ff_C +1;
    $namespace_map->{$cname} = "SIMINT_CVIC_$cname";


    # collecting all of the outputs to include with the simulation outputs
    # Covariate initial condition used
    $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."SIMINT_CVIC_$cname".&fetch_padding("SIMINT_CVIC_$cname", $cfg->{species_length})." = $counter_ode_outputs \n";
    $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = SIMINT_CVIC_$cname;\n";
    $counter_ode_outputs = 1 + $counter_ode_outputs;

    # Covariate value
    $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."$cname".&fetch_padding($cname, $cfg->{species_length})." = $counter_ode_outputs \n";
    $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = $cname;\n";
    $counter_ode_outputs = 1 + $counter_ode_outputs;



    foreach $set (keys(%{$cfg->{covariates}->{$cname}->{parameter_sets}})) {
      $mc->{FETCH_SYS_COVARIATES} .= 'cfg$options$inputs$covariates$'.$cname.'$parameter_sets$'.$set.'$times  = '."c(".join(', ', &extract_elements($cfg->{covariates}->{$cname}->{parameter_sets}->{$set}->{times})).")\n"; 
      $mc->{FETCH_SYS_COVARIATES} .= 'cfg$options$inputs$covariates$'.$cname.'$parameter_sets$'.$set.'$values = '."c(".join(', ', &extract_elements($cfg->{covariates}->{$cname}->{parameter_sets}->{$set}->{values})).")\n"; 
    }
    $mc->{FETCH_SYS_COVARIATES} .= "\n";
  }
}

# static secondary parameters
if ((@{$cfg->{static_secondary_parameters_index}})){
  $mo->{OUTPUTS_REMAP}      .= "/* Secondary Parameters (Static) */\n";
  foreach $parameter    (@{$cfg->{static_secondary_parameters_index}}){
    $pname = $parameter;
    # R script
    $mc->{FETCH_SYS_SSP} .= 'cfg$options$ssp$'.$pname.'="'.&apply_format($cfg, $cfg->{static_secondary_parameters}->{$pname}, 'rproject').'"'."\n";
    $mc->{SS_PARAM} .= $pname.&fetch_padding($pname, $cfg->{parameters_length})." = ";
    $mc->{SS_PARAM} .= &apply_format($cfg, $cfg->{static_secondary_parameters}->{$pname}, 'rproject')." \n"; 
    if(defined($cfg->{if_conditional}->{$pname})){
      $mc->{SS_PARAM} .= &extract_conditional($cfg, $pname, 'rproject');
    }
    
    # Compiled code level
    if($mo->{SS_PARAM} eq ""){
      $mo->{VARIABLE_INIT} .= "/* Static Secondary Parameters*/\n";
      $mo->{SS_PARAM} .= "/* Static Secondary Parameters*/\n";
    }
    $mo->{VARIABLE_INIT} .= "double $pname ".&fetch_padding($pname, $cfg->{parameters_length})." = 0.0;\n";
    $mo->{SS_PARAM} .= $pname.&fetch_padding($pname, $cfg->{parameters_length})." = ";
    $mo->{SS_PARAM} .= &apply_format($cfg, $cfg->{static_secondary_parameters}->{$pname}, 'C')."; \n"; 
    if(defined($cfg->{if_conditional}->{$pname})){
      $mo->{SS_PARAM} .= &extract_conditional($cfg, $pname, 'C');
    }

  # collecting all of the outputs to include with the simulation outputs
  $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."$parameter".&fetch_padding($parameter, $cfg->{species_length})." = $counter_ode_outputs \n";
  $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = $parameter;\n";
  $counter_ode_outputs       = 1 + $counter_ode_outputs;


  }
  # if any covariates were used in the static secondary parameters, we change
  # those to the SIMINT_CVIC_ versions which are just the covariates evaluated 
  # at their initial values.
  $mo->{SS_PARAM} = &remap_namespace($mo->{SS_PARAM}, $namespace_map);

}

# dynamic secondary parameters
if ((@{$cfg->{dynamic_secondary_parameters_index}})){
  $mo->{OUTPUTS_REMAP}      .= "/* Secondary Parameters (Dynamic) */\n";
  foreach $parameter    (@{$cfg->{dynamic_secondary_parameters_index}}){
    $pname = $parameter;
    # R script
    $mc->{FETCH_SYS_DSP} .= 'cfg$options$dsp$'.$parameter.'="'.&apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$parameter}, 'rproject').'"'."\n";
    $mc->{DS_PARAM} .= $pname.&fetch_padding($pname, $cfg->{parameters_length})." = ";
    $mc->{DS_PARAM} .= &apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$pname}, 'rproject')." \n"; 
    if(defined($cfg->{if_conditional}->{$pname})){
      $mc->{DS_PARAM} .= &extract_conditional($cfg, $pname, 'rproject');
    }

    # Compiled code level
    if($mo->{DS_PARAM} eq ""){
      $mo->{VARIABLE_INIT} .= "/* Dynamic Secondary Parameters*/\n";
      $mo->{DS_PARAM} .= "/* Dynamic Secondary Parameters*/\n";
    }
    $mo->{VARIABLE_INIT} .= "double $pname ".&fetch_padding($pname, $cfg->{parameters_length})." = 0.0;\n";
    $mo->{DS_PARAM} .= $pname.&fetch_padding($pname, $cfg->{parameters_length})." = ";
    $mo->{DS_PARAM} .= &apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$pname}, 'C')."; \n"; 
    if(defined($cfg->{if_conditional}->{$pname})){
      $mo->{DS_PARAM} .= &extract_conditional($cfg, $pname, 'C');
    }

  # collecting all of the outputs to include with the simulation outputs
  $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."$parameter".&fetch_padding($parameter, $cfg->{species_length})." = $counter_ode_outputs \n";
  $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = $parameter;\n";
  $counter_ode_outputs       = 1 + $counter_ode_outputs;



  }
}

# dumping the miscellaneous options
if(keys %{$cfg->{options}}){
  foreach $option (keys %{$cfg->{options}}){
    $mc->{FETCH_SYS_MISC} .= 'cfg$options$misc$'.$option.' = '."'".&apply_format($cfg, $cfg->{options}->{$option}, 'rproject')."' ;\n" 
  }
}


# time scales
$mc->{FETCH_SYS_TS} .= 'cfg$options$time_scales$time'.&fetch_padding("time", $cfg->{time_scales_length})." = 1.0\n";

# Adding the default "time" timescale 
$mo->{OUTPUTS_REMAP}      .= "/* Time Scales */\n";
$mc->{TIME_SCALES}        .= "ts.time           = SIMINT_TIME;\n";
$mo->{TIME_SCALES}        .= "SIMINT_TS_time    = SIMINT_TIME;\n";
$mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."ts.time".&fetch_padding("ts.time", $cfg->{species_length})." = $counter_ode_outputs \n";
$mo->{VARIABLE_INIT}.= "double SIMINT_TS_time".&fetch_padding("SIMINT_TS_time", $cfg->{outputs_length})." = 0.0;\n";
$mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = SIMINT_TS_time;\n";
$counter_ode_outputs = 1 + $counter_ode_outputs;

if ((@{$cfg->{time_scales_index}})){
  foreach $option       (@{$cfg->{time_scales_index}}){
    $mc->{FETCH_SYS_TS} .= 'cfg$options$time_scales$'.$option.&fetch_padding($option, $cfg->{time_scales_length})." = ";
    $mc->{FETCH_SYS_TS} .= $cfg->{time_scales}->{$option}."\n";

  # # collecting all of the outputs to include with the simulation outputs
    $mo->{VARIABLE_INIT}.= "double SIMINT_TS_$option".&fetch_padding("SIMINT_TS_$option", $cfg->{outputs_length})." = 0.0;\n";
    $mc->{FETCH_SYS_INDICES_ODE_OUTPUT} .= 'cfg$options$mi$odes$'."ts.$option".&fetch_padding("ts.$option", $cfg->{species_length})." = $counter_ode_outputs \n";
    $mo->{TIME_SCALES}        .= "SIMINT_TS_$option = SIMINT_TIME*$cfg->{time_scales}->{$option};\n";
    $mc->{TIME_SCALES}        .= "ts.$option = SIMINT_TIME*$cfg->{time_scales}->{$option};\n";
    $mo->{OUTPUTS_REMAP}      .= "yout[".($counter_ode_outputs-1)."] = SIMINT_TS_$option;\n";
    $counter_ode_outputs = 1 + $counter_ode_outputs;
  }
}

if((@{$cfg->{data}->{headers}->{values}})){
  $mc->{FETCH_SYS_DATA} .= 'cfg = system_load_data(cfg, dsname     = "default",'."\n";
  $mc->{FETCH_SYS_DATA} .= '                            data_file  = "'.$cfg->{data}->{file}.'"'.")\n";
}
else{
  $mc->{FETCH_SYS_DATA} .= "# No data file specified\n";}

if ((@{$cfg->{parameter_sets_index}})){
  $md->{PSETS} .= "# set name".fetch_padding("set name", 25)." | Description\n";
  $md->{PSETS} .= "# -------------------------------------------------------\n";
  foreach $set (@{$cfg->{parameter_sets_index}}){
    # dumping the name
    $mc->{FETCH_SYS_PSETS}   .= 'cfg$parameters$sets$'.$set.'$name   = '."'$cfg->{parameter_sets}->{$set}->{name}'\n";
    # assigning the values to the default value
    $mc->{FETCH_SYS_PSETS}   .= 'cfg$parameters$sets$'.$set.'$values = cfg$parameters$matrix$value'."\n";
    #overwriting those unique to this set
    foreach $parameter (keys(%{$cfg->{parameter_sets}->{$set}->{values}})){
      $pname = $parameter;
      $mc->{FETCH_SYS_PSETS}   .= 'cfg$parameters$sets$'.$set.'$values[cfg$options$mi$parameters$'.$pname.']';
      $mc->{FETCH_SYS_PSETS}   .= &fetch_padding($pname, $cfg->{parameters_length})." = ".$cfg->{parameter_sets}->{$set}->{values}->{$pname}."\n";

    }

    $md->{PSETS} .= "# $set".fetch_padding($set, 25)." | $cfg->{parameter_sets}->{$set}->{name}\n";
  }

  $ma->{PSETS} = $md->{PSETS};
}


if (defined($cfg->{bolus_inputs}->{entries})){
  # Recording the bolus times
  $field_times = $cfg->{bolus_inputs}->{times}->{values};
  $field_times =~ s#\[##g; $field_times =~ s#\]##g; 

  # simulation driver
  $md->{BOLUS} .= "# To overwrite the default dosing uncomment the following\n";
  # $md->{BOLUS} .= '# cfg$options$inputs$bolus$times$values                  = c('.$field.') # '.$cfg->{bolus_inputs}->{times}->{units}."\n";
  
  $md->{BOLUS} .= "# Setting all dosing to zero\n";
  $md->{BOLUS} .= "# cfg = system_zero_inputs(cfg) \n";

  # system information
  $mc->{FETCH_SYS_BOLUS} .= 'cfg$options$inputs$bolus$times$values = c('.join(', ', &extract_elements($field_times)).")\n";
  $mc->{FETCH_SYS_BOLUS} .= 'cfg$options$inputs$bolus$times$scale ='."'$cfg->{bolus_inputs}->{times}->{scale}'\n";
  $mc->{FETCH_SYS_BOLUS} .= 'cfg$options$inputs$bolus$times$units ='."'$cfg->{bolus_inputs}->{times}->{units}'\n";
  # for each state that gets a bolus we make three entries: values, scaling
  # information and the expected units
  foreach $state (keys(%{$cfg->{bolus_inputs}->{entries}})){
  $field_state = $cfg->{bolus_inputs}->{entries}->{$state}->{values};
  $field_state =~ s#\[##g; $field_state =~ s#\]##g; 
    # system information
    $mc->{FETCH_SYS_BOLUS} .= 'cfg$options$inputs$bolus$species$'.$state.'$values = c('.join(', ', &extract_elements($field_state)).")\n";
    $mc->{FETCH_SYS_BOLUS} .= 'cfg$options$inputs$bolus$species$'.$state.'$scale  ='."'$cfg->{bolus_inputs}->{entries}->{$state}->{scale}'\n";
    $mc->{FETCH_SYS_BOLUS} .= 'cfg$options$inputs$bolus$species$'.$state.'$units  ='."'$cfg->{bolus_inputs}->{entries}->{$state}->{units}'\n";

    # simulation driver
    $md->{BOLUS} .= '# cfg = system_set_bolus(cfg, state   ="'.$state.'", '."\n" ;
    $md->{BOLUS} .= '#                             times   = c('.$field_times.'),  #  '.$cfg->{bolus_inputs}->{times}->{units}."\n";
    $md->{BOLUS} .= '#                             values  = c('.$field_state.'))  #  '.$cfg->{bolus_inputs}->{entries}->{$state}->{units}."\n";
    
    # analysis estimation
    $ma->{BOLUS}          .= 'cohort$inputs$bolus$'.$state.'$TIME'.&fetch_padding($state,19).'= c()'." # $cfg->{bolus_inputs}->{times}->{units} \n";
    $ma->{BOLUS}          .= 'cohort$inputs$bolus$'.$state.'$AMT'.&fetch_padding($state,20).'= c()'." # $cfg->{bolus_inputs}->{entries}->{$state}->{units} \n";
  }
  $mc->{FETCH_SYS_BOLUS} .=  "\n\n\n";
}
# adding the comments to the top of the file
$mc->{COMMENTS} .= &fetch_comments($cfg->{comments}, 'rproject');
# $mc->{FETCH_SYS} .= 'cfg$parameters$sets$'.default.' = cfg$parameters$matrix$value'


 foreach $field     (keys(%{$mc})){
     $template_components =~ s#<$field>#$mc->{$field}#g;
 }

 open(FH, '>', &ftf($cfg, $cfg->{files}->{rproject}->{components}));
 print FH $template_components;
 close(FH);


 foreach $field     (keys(%{$md})){
     $template_driver  =~ s#<$field>#$md->{$field}#g;
 }
 open(FH, '>', &ftf($cfg, $cfg->{files}->{rproject}->{simulation_driver}));
 print FH $template_driver;
 close(FH);

 foreach $field     (keys(%{$mo})){
     $template_compiled   =~ s#<$field>#$mo->{$field}#g;
 }
 open(FH, '>', &ftf($cfg, $cfg->{files}->{rproject}->{compiled}));
 print FH $template_compiled;
 close(FH);

 foreach $field     (keys(%{$ma})){
     $template_analysis  =~ s#<$field>#$ma->{$field}#g;
 }
 open(FH, '>', &ftf($cfg, $cfg->{files}->{rproject}->{analysis_estimation}));
 print FH $template_analysis;
 close(FH);


}



sub dump_adapt
{
  my ($cfg, $parameter_set) = @_;
  my $name      = '';
  my $counter   = 0;

  my @ode_names = ();
  my @var_names = ();

  my $text_string = '';

  my $tmp_ode   = '';

  my $template_fortran  = &fetch_template_file($cfg, 'adapt_system.for');
  my $template_prm      = &fetch_template_file($cfg, 'adapt_parameters.prm');
  # hash to hold the model components
  my $mc = {};


  # the order here is important because prm_file expects a
  # specific order
  if ((@{$cfg->{species_index}})){
    $mc->{SYMBOL_NDEqs}     = scalar(@{$cfg->{species_index}});}
  else{
    $mc->{SYMBOL_NDEqs}     = '0';}

  if ((@{$cfg->{parameters_system_index}})){
    $mc->{SYMBOL_NSParam}   = scalar(@{$cfg->{parameters_system_index}});}
  else{
    $mc->{SYMBOL_NSParam}   = '0';}

  if ((@{$cfg->{parameters_variance_index}})){
    $mc->{SYMBOL_NVParam}   = scalar(@{$cfg->{parameters_variance_index}});}
  else{
    $mc->{SYMBOL_NVParam}   = '0';}

  if ((@{$cfg->{static_secondary_parameters_index}})){
    $mc->{SYMBOL_NSecPar}   = scalar(@{$cfg->{static_secondary_parameters_index}});}
  else{
    $mc->{SYMBOL_NSecPar}   = '0';}

  # This is for the covariance parameters and should be 
  # changed when we add the population component
  $mc->{SYMBOL_NCVParam}   = '0';

  # Name placeholders
  $mc->{SYMBOL_PARAMETER_NAMES}                              = '';
  $mc->{SYMBOL_VARIANCE_PARAMETER_NAMES}                     = '';
  $mc->{SYMBOL_SECONDARY_PARAMETER_NAMES}                    = '';

  # variable declaration place holders
  $mc->{COMMON_BLOCK_DECLARE_PARAMETERS}                     = '';
  $mc->{COMMON_BLOCK_DECLARE_STATIC_SECONDARY_PARAMETERS}    = '';
  $mc->{COMMON_BLOCK_DECLARE_STATE_DEFINITIONS}              = '';
  $mc->{COMMON_BLOCK_DECLARE_DYNAMIC_SECONDARY_PARAMETERS}   = '';
  $mc->{COMMON_BLOCK_DECLARE_OUPUT_DEFINITION}               = '';
  $mc->{COMMON_BLOCK_DECLARE_VARIANCE_DEFINITIONS}           = '';
  $mc->{COMMON_BLOCK_DECLARE_VARIANCE_EQUATION_DEFINITIONS}  = '';
  $mc->{COMMON_BLOCK_DECLARE_INFUSION_RATE_DEFINITIONS}      = '';

  # place holders for mapping fotran variables 
  # named variables
  $mc->{COMMON_BLOCK_PARAMETERS}                             = '';
  $mc->{COMMON_BLOCK_STATIC_SECONDARY_PARAMETERS}            = '';
  $mc->{COMMON_BLOCK_STATE_DEFINITIONS}                      = '';
  $mc->{COMMON_BLOCK_DYNAMIC_SECONDARY_PARAMETERS}           = '';
  $mc->{COMMON_BLOCK_OUTPUT_DEFINITIONS}                     = '';
  $mc->{COMMON_BLOCK_VARIANCE_DEFINITIONS}                   = '';
  $mc->{COMMON_BLOCK_VARIANCE_EQUATION_DEFINITIONS}          = '';
  $mc->{COMMON_BLOCK_INFUSION_RATE_DEFINITIONS}              = '';
                                       
  # variable assignment and mapping back to fortran variables
  $mc->{ODES_ASSIGNMENT}                                     = '';
  $mc->{ODES_MAP}                                            = '';
                                                            
  $mc->{OUTPUTS_ASSIGNMENT}                                  = '';
  $mc->{OUTPUTS_MAP}                                         = '';
                                                            
  $mc->{SECONDARY_PARAMETERS_ASSIGNMENT}                     = '';
  $mc->{SECONDARY_PARAMETERS_MAP}                            = '';
                                                            
  $mc->{VARIANCES_ASSIGNMENT}                                = '';
  $mc->{VARIANCES_MAP}                                       = '';

  # values inserted into the prm file
  $mc->{VALUES_PARAMETERS}                                   = '';
  $mc->{VALUES_IC}                                           = '';
  $mc->{VALUES_VARIANCE_PARAMETERS}                          = '';

  #
  # Processing system parameters
  #
  if ((@{$cfg->{parameters_system_index}})){
    $mc->{COMMON_BLOCK_PARAMETERS}         .= "C---->System Parameters\n";
    $mc->{COMMON_BLOCK_DECLARE_PARAMETERS} .= "C---->Declaring System Parameters\n";
    $mc->{COMMON_BLOCK_DECLARE_PARAMETERS} .= &fortranify_line("Real*8 ".join(', ', @{$cfg->{parameters_system_index}}));
    $counter = 1;
    foreach $name (@{$cfg->{parameters_system_index}}){

      # if the current parameter set overwrites a particular value then
      # we dump the new value
      if(exists($cfg->{parameter_sets}->{$parameter_set}->{values}->{$name})){
        $mc->{VALUES_PARAMETERS}       .= $cfg->{parameter_sets}->{$parameter_set}->{values}->{$name};}
      # otherwise we just dump the original value
      else{
        $mc->{VALUES_PARAMETERS}       .= $cfg->{parameters}->{$name}->{value}; }

      if($counter < (@{$cfg->{parameters_system_index}})){
         $mc->{VALUES_PARAMETERS}       .= "\n"; }

      $mc->{SYMBOL_PARAMETER_NAMES}  .= &fortranify_line("Psym($counter) = '$name'");
      $mc->{COMMON_BLOCK_PARAMETERS} .= &fortranify_line("$name ".&fetch_padding($name, $cfg->{parameters_length})."= P($counter)");
      $counter = $counter + 1;
    }
  }

  #
  # Processing static secondary parameters
  #
  if ((@{$cfg->{static_secondary_parameters_index}})){
    $mc->{SECONDARY_PARAMETERS_ASSIGNMENT}                  .= "C---->Assigning Secondary Parameters\n";
    $mc->{SECONDARY_PARAMETERS_MAP}                         .= "C---->Mapping Secondary Parameters to PS variables\n";
    $mc->{COMMON_BLOCK_STATIC_SECONDARY_PARAMETERS}         .= "C---->Secondary Parameters\n";
    $mc->{COMMON_BLOCK_DECLARE_STATIC_SECONDARY_PARAMETERS} .= "C---->Declaring Secondary Parameters\n";
    $mc->{COMMON_BLOCK_DECLARE_STATIC_SECONDARY_PARAMETERS} .= &fortranify_line("Real*8 ".join(', ', @{$cfg->{static_secondary_parameters_index}}));
    $counter = 1;
    foreach $name (@{$cfg->{static_secondary_parameters_index}}){
      $mc->{SYMBOL_SECONDARY_PARAMETER_NAMES} .= &fortranify_line("PSsym($counter) = '$name'");
      $mc->{COMMON_BLOCK_STATIC_SECONDARY_PARAMETERS} .= &fortranify_line("$name ".&fetch_padding($name, $cfg->{parameters_length})."= PS($counter)");

      $text_string = $cfg->{static_secondary_parameters}->{$name};
      # converting generic functions into fortran functions
      $text_string = &apply_format($cfg, $text_string, 'fortran');
      if(defined($cfg->{if_conditional}->{$name})){
        $mc->{SECONDARY_PARAMETERS_ASSIGNMENT} .= &extract_conditional($cfg, $name, 'fortran');
        }
      else{
        $mc->{SECONDARY_PARAMETERS_ASSIGNMENT} .= &fortranify_line("$name ".&fetch_padding($name, $cfg->{parameters_length})."= $text_string");
        }



      $mc->{SECONDARY_PARAMETERS_MAP}        .= &fortranify_line("PS($counter) ".&fetch_padding("PS($counter)", 8)."= $name");
       
      $counter = $counter + 1;
    }
  }



  if ((keys(%{$cfg->{input_rates}}))){
    $mc->{COMMON_BLOCK_DECLARE_INFUSION_RATE_DEFINITIONS}  .= "C---->Declaring Infusion Rates\n";
    $mc->{COMMON_BLOCK_DECLARE_INFUSION_RATE_DEFINITIONS}  .= &fortranify_line("Real*8 ".join(', ', keys(%{$cfg->{input_rates}})));
    $mc->{COMMON_BLOCK_INFUSION_RATE_DEFINITIONS}          .= "C---->Infusion Rates\n";
    $counter = 1;
    foreach $name (keys(%{$cfg->{input_rates}})){
      $mc->{COMMON_BLOCK_INFUSION_RATE_DEFINITIONS}        .= &fortranify_line("$name ".&fetch_padding($name, $cfg->{parameters_length})."= R($counter)");
      $counter = $counter + 1;
  
    }
  }

  #
  # Processing dynamic secondary parameters
  #
   

  $mc->{COMMON_BLOCK_DECLARE_DYNAMIC_SECONDARY_PARAMETERS} .= "C---->Declaring Dynamic Secondary Parameters\n";
  $mc->{COMMON_BLOCK_DECLARE_DYNAMIC_SECONDARY_PARAMETERS} .= &fortranify_line("Real*8 SIMINT_TIME");
  $mc->{COMMON_BLOCK_DYNAMIC_SECONDARY_PARAMETERS}         .= "C---->Dynamic Secondary Parameters\n";
  $mc->{COMMON_BLOCK_DYNAMIC_SECONDARY_PARAMETERS}         .= &fortranify_line("SIMINT_TIME ".&fetch_padding("SIMINT_TIME", $cfg->{parameters_length})."= T");
   
  if ((@{$cfg->{dynamic_secondary_parameters_index}})){
    $mc->{COMMON_BLOCK_DECLARE_DYNAMIC_SECONDARY_PARAMETERS} .= &fortranify_line("Real*8 ".join(', ', @{$cfg->{dynamic_secondary_parameters_index}}));
    $counter = 1;
    foreach $name (@{$cfg->{dynamic_secondary_parameters_index}}){
      $counter = $counter + 1;

      $text_string = $cfg->{dynamic_secondary_parameters}->{$name};
      # converting generic functions into fortran functions
      $text_string = &apply_format($cfg, $text_string, 'fortran');
      $mc->{COMMON_BLOCK_DYNAMIC_SECONDARY_PARAMETERS} .= &fortranify_line("$name ".&fetch_padding($name, $cfg->{parameters_length})."= $text_string");

      if(defined($cfg->{if_conditional}->{$name})){
        $mc->{COMMON_BLOCK_DYNAMIC_SECONDARY_PARAMETERS} .= &extract_conditional($cfg, $name, 'fortran');
      }

    }
  }

  #
  # Processing states and odes 
  #
  if ((@{$cfg->{species_index}})){
    $mc->{COMMON_BLOCK_STATE_DEFINITIONS}         .= "C---->States\n";
    $mc->{COMMON_BLOCK_DECLARE_STATE_DEFINITIONS} .= "C---->Declaring State and Derivative Names \n";
    $mc->{COMMON_BLOCK_DECLARE_STATE_DEFINITIONS} .= &fortranify_line("Real*8 ".join(', ', @{$cfg->{species_index}}));
    $mc->{ODES_ASSIGNMENT}                        .= "C---->Assigning the ODEs\n";
    $mc->{ODES_MAP}                               .= "C---->Mapping Named ODEs to the XP variables \n";
    $counter = 1;
    foreach $name (@{$cfg->{species_index}}){
      # setting the initial condition in the prm file to zero
      $mc->{VALUES_IC}                              .= "0.0";
      if($counter < (@{$cfg->{species_index}})){
         $mc->{VALUES_IC}               .= "\n"; }
      # Initializing the temporary ode string

      # if a nonzero initial condition has been specified for this state then we have to
      # add it in here:
      if(defined($cfg->{initial_conditions}->{$name})){
         $mc->{COMMON_BLOCK_STATE_DEFINITIONS} .= &fortranify_line("$name ".&fetch_padding($name, $cfg->{species_length})."= X($counter) + $cfg->{initial_conditions}->{$name}");}
      # otherwise we just mapp the X() parameter directly here
      else{
         $mc->{COMMON_BLOCK_STATE_DEFINITIONS} .= &fortranify_line("$name ".&fetch_padding($name, $cfg->{species_length})."= X($counter)");}
      # storing the ode names for use below
      push @ode_names, "SIMINT_d$name";
   
      $tmp_ode = &make_ode($cfg, $name, 'fortran');

      # prepending the assignment portion of the ODE
      $tmp_ode   = "SIMINT_d$name".&fetch_padding("SIMINT_d$name", $cfg->{species_length})." = ".$tmp_ode;

      # adding the ODE to the ODEs string
      $mc->{ODES_ASSIGNMENT}  .= &fortranify_line($tmp_ode);

      #Mapping the defined ODEs to the XP variables
      $mc->{ODES_MAP} .= &fortranify_line("XP($counter)".&fetch_padding("XP($counter)", 8)."= SIMINT_d$name");

      $counter = $counter + 1;
    }
    $mc->{COMMON_BLOCK_DECLARE_STATE_DEFINITIONS} .= &fortranify_line("Real*8 ".join(', ', @ode_names));
  }

  #
  # Processing states and odes 
  #
  if((@{$cfg->{outputs_index}})){
    $mc->{VARIANCE_MAP}                            .= "C---->Mapping Variance to V variables \n" ;
    $mc->{VARIANCE_ASSIGNMENT}                     .= "C---->Assigning Variances \n";
    $mc->{OUTPUTS_MAP}                             .= "C---->Mapping Outputs to Y variables \n" ;
    $mc->{OUTPUTS_ASSIGNMENT}                      .= "C---->Assigning Outputs \n";
    $mc->{COMMON_BLOCK_DECLARE_OUTPUT_DEFINITIONS} .= "C---->Declaring Outputs \n";
    $mc->{COMMON_BLOCK_DECLARE_OUTPUT_DEFINITIONS} .= &fortranify_line("Real*8 ".join(', ', @{$cfg->{outputs_index}}));
    $mc->{COMMON_BLOCK_OUTPUT_DEFINITIONS}         .= "C---->Defining Outputs \n";
    $counter   = 1;
    @var_names = ();


    # used to map PRED to actual values
    my $nsmap;

    foreach $name ( &fetch_estimateable_outputs($cfg)){
      $mc->{COMMON_BLOCK_OUTPUT_DEFINITIONS} .= &fortranify_line("$name".&fetch_padding("$name", $cfg->{outputs_length})."= Y($counter)");
      $text_string = $cfg->{outputs}->{$name};
      # converting generic functions into fortran functions
      $text_string = &apply_format($cfg, $text_string, 'fortran');


      $mc->{OUTPUTS_ASSIGNMENT}              .= &fortranify_line("$name".&fetch_padding("$name", $cfg->{outputs_length})."= $text_string");
      $mc->{OUTPUTS_MAP}                     .= &fortranify_line("Y($counter)".&fetch_padding("Y($counter)", 8)."= $name");


      push @var_names, "SIMINT_VAR_$name";
      # a variance equation has been specified for this output
      if(defined($cfg->{variance}->{equations}->{$name})){
         $text_string = $cfg->{variance}->{equations}->{$name};
         # converting generic functions into fortran functions
         $text_string = &apply_format($cfg, $text_string, 'fortran');
         # replacing PRED with the actual values
         $nsmap->{PRED} = $name;
         $text_string = &remap_namespace($text_string, $nsmap);
      }
      else{ $text_string = '1.0';}


      $mc->{VARIANCE_ASSIGNMENT}             .= &fortranify_line("SIMINT_VAR_$name".&fetch_padding("SIMINT_VAR_$name", 20)."= ".$text_string);
      $mc->{VARIANCE_MAP}                    .= &fortranify_line("V($counter)".&fetch_padding("V($counter)", 8)."= SIMINT_VAR_$name");

      $counter = $counter + 1;
    }

    $mc->{COMMON_BLOCK_DECLARE_VARIANCE_EQUATION_DEFINITIONS} .=  "C---->Declaring Variance Equation Variables \n";
    $mc->{COMMON_BLOCK_DECLARE_VARIANCE_EQUATION_DEFINITIONS} .= &fortranify_line("Real*8 ".join(', ', @var_names));

  }

  #
  # Processing variance parameters
  #
  if ((@{$cfg->{parameters_variance_index}})){
    $mc->{COMMON_BLOCK_VARIANCE_DEFINITIONS}         .= "C---->Variance Parameters\n";
    $mc->{COMMON_BLOCK_DECLARE_VARIANCE_DEFINITIONS} .= "C---->Declaring Variance Parameters\n";
    $mc->{COMMON_BLOCK_DECLARE_VARIANCE_DEFINITIONS} .= &fortranify_line("Real*8 ".join(', ', @{$cfg->{parameters_variance_index}}));
    $counter = 1;
    foreach $name (@{$cfg->{parameters_variance_index}}){
      $mc->{COMMON_BLOCK_VARIANCE_DEFINITIONS} .= &fortranify_line("$name".&fetch_padding("$name", $cfg->{parameters_length})."= PV($counter)");

      # appending values to the place holder for variance parameters
      $mc->{VALUES_VARIANCE_PARAMETERS}        .= $cfg->{parameters}->{$name}->{value};
      if($counter < (@{$cfg->{parameters_variance_index}})){
         $mc->{VALUES_VARIANCE_PARAMETERS}     .= "\n"; }
      $counter = $counter + 1;
    }

  }






  foreach $name      (keys(%{$mc})){
      $template_fortran =~ s#<$name>#$mc->{$name}#g;
      $template_prm     =~ s#<$name>#$mc->{$name}#g;
  }

  #print(&catfile($cfg->{files}->{temp_directory}, $cfg->{files}->{adapt}.".for"));

  
  
  open(FH, '>', &ftf($cfg, $cfg->{files}->{adapt}.".for"));
  print FH $template_fortran;
  close(FH);

  open(FH, '>', &ftf($cfg, $cfg->{files}->{adapt}."-$parameter_set.prm")); 
  print FH $template_prm;
  close(FH);
}

# fetching the temporary file name
sub ftf {
  my ($cfg, $tmpfile) = @_;
  return (&catfile($cfg->{files}->{temp_directory}, $tmpfile));
}

sub dump_monolix
{
  my ($cfg, $parameter_set) = @_;
  my $species   = '';
  my $parameter = '';
  my $output    = '';
  my $name      = '';
  my $tmp_ode   = '';

  my $iiv_dist  = '';
  my $iiv_var   = '';
  
  my $iiv_ETA     = '';
  my $iiv_set     = 'default';
  if($cfg->{iiv_index}->{$parameter_set}){
     $iiv_set = $parameter_set; 
  }

  my $template = &fetch_template_file($cfg, 'monolix.txt');
  # hash to hold the model components
  my $mc = {};

  #initializing the model components
  $mc->{INPUT}        = '';
  $mc->{BOLUS}        = '';
  $mc->{EQUATION}     = '';
  $mc->{POPULATION}   = '';
  $mc->{DESCRIPTION}  = '';
  
  $mc->{DESCRIPTION}  = &fetch_comments($cfg->{comments}, 'monolix');

  # dumping the system parameters
  if ((@{$cfg->{parameters_system_index}})){
    $mc->{INPUT} .=  "input = {".join(', ', @{$cfg->{parameters_system_index}})."}\n";
    foreach $name (@{$cfg->{parameters_system_index}}){
      # Default variance information for parameters
      $iiv_var  = '0.1';
      $iiv_ETA  = '';
      $iiv_dist = 'logNormal,';
      # For variance values that have been specified we add them here
      if(defined($cfg->{iiv}->{$iiv_set}->{parameters}->{$name})){
        if($cfg->{iiv}->{$iiv_set}->{parameters}->{$name}->{distribution} eq "LN"){
          $iiv_dist = 'logNormal,';
        }
        elsif($cfg->{iiv}->{$iiv_set}->{parameters}->{$name}->{distribution} eq "N"){
          $iiv_dist = 'Normal,   ';
        }
        $iiv_ETA = $cfg->{iiv}->{$iiv_set}->{parameters}->{$name}->{iiv_name};
        if(defined($cfg->{iiv}->{$iiv_set}->{vcv}->{$iiv_ETA}->{$iiv_ETA})){
          $iiv_var = $cfg->{iiv}->{$iiv_set}->{vcv}->{$iiv_ETA}->{$iiv_ETA};
        }
      }


      $mc->{POPULATION} .= "pop_{$name} ".&fetch_padding($name, $cfg->{parameters_length})."= {distribution=$iiv_dist median=";
      if(exists($cfg->{parameter_sets}->{$parameter_set}->{values}->{$name})){
        $mc->{POPULATION} .= $cfg->{parameter_sets}->{$parameter_set}->{values}->{$name}.", ".&fetch_padding("$cfg->{parameter_sets}->{$parameter_set}->{values}->{$name}", $cfg->{parameter_values_length}); }
      else{
        $mc->{POPULATION} .= $cfg->{parameters}->{$name}->{value}.", ".&fetch_padding("$cfg->{parameters}->{$name}->{value}", $cfg->{parameter_values_length}) ; }
      $mc->{POPULATION} .= " variance=$iiv_var} \n";
    }
  }

  #
  # STATIC SECONDARY PARAMTERS
  #
  if ((@{$cfg->{static_secondary_parameters_index}})){
    $mc->{EQUATION} .=  "\n;-->Static Secondary Parameters \n";
    foreach $name (@{$cfg->{static_secondary_parameters_index}}){
      $mc->{EQUATION} .= "$name ".&fetch_padding($name, $cfg->{parameters_length})."= ".&apply_format($cfg, $cfg->{static_secondary_parameters}->{$name}, 'monolix')."\n";
    }
  }

  #
  # NONZERO INITIAL CONDITIONS 
  #
  if(keys %{$cfg->{initial_conditions}}){
    $mc->{EQUATION} .=  "\n;-->Nonzero Initial Conditions  \n";
    $mc->{EQUATION} .=  "t0 " .&fetch_padding("t0", $cfg->{species_length})."= 0 \n";
    foreach $species (keys(%{$cfg->{initial_conditions}})){
      $mc->{EQUATION} .=  $species."_0 ".&fetch_padding("$species  ", $cfg->{species_length})."= ".&apply_format($cfg, $cfg->{initial_conditions}->{$species}, 'monolix')." \n";     
    }
  }

  if (defined($cfg->{bolus_inputs}->{entries})){
    $mc->{BOLUS} =  "PK:\n";
    foreach $name  (keys(%{$cfg->{bolus_inputs}->{entries}})){
       $mc->{BOLUS} .= "depot(adm=1, target=$name, p=".$cfg->{bolus_inputs}->{entries}->{$name}->{scale}.")\n";
    }

  }



  #
  # DYNAMIC SECONDARY PARAMTERS
  #
  $mc->{EQUATION} .=  "\n;-->Dynamic Secondary Parameters \n";
  $mc->{EQUATION} .=  "SIMINT_TIME " .&fetch_padding("SIMINT_TIME", $cfg->{species_length})."= t \n";
  if ((@{$cfg->{dynamic_secondary_parameters_index}})){
    foreach $name (@{$cfg->{dynamic_secondary_parameters_index}}){
      # if the parameter is defined by a conditional then
      # we extract that here otherwise, we just use a simple assignment
      if(defined($cfg->{if_conditional}->{$name})){
        $mc->{EQUATION} .= &extract_conditional($cfg, $name, 'monolix'); }
      else{
      $mc->{EQUATION} .= "$name ".&fetch_padding($name, $cfg->{parameters_length})."= ".&apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$name}, 'monolix')."\n"; }
    }
  }

  #
  # ODES                        
  #
  if ((@{$cfg->{species_index}})){
    $mc->{EQUATION} .=  "\n;-->ODEs \n";
    foreach $name (@{$cfg->{species_index}}){
      $tmp_ode   = "";
      if((@{$cfg->{species}->{$name}->{odes}})){
        if($tmp_ode eq ""){
          $tmp_ode .= join('+', @{$cfg->{species}->{$name}->{odes}}); }
        else{
          $tmp_ode .= "+".join('+', @{$cfg->{species}->{$name}->{odes}}); }
       }

      if((@{$cfg->{species}->{$name}->{production}})){
       if($tmp_ode eq ""){
         $tmp_ode .= join('+', @{$cfg->{species}->{$name}->{production}}); }
       else{
         $tmp_ode .= "+".join('+', @{$cfg->{species}->{$name}->{production}}); }
      }

      if((@{$cfg->{species}->{$name}->{consumption}})){
         $tmp_ode .= "-(".join('+', @{$cfg->{species}->{$name}->{consumption}}).")"; }

      # converting generic functions into fortran functions
      $tmp_ode   = &apply_format($cfg, $tmp_ode, 'monolix');
       
      # prepending the assignment portion of the ODE
      $tmp_ode   = "ddt_$name".&fetch_padding("ddt_$name ", $cfg->{species_length})."= ".$tmp_ode;

      $mc->{EQUATION} .= $tmp_ode."\n";
    }
  }


  if((@{$cfg->{outputs_index}})){
    $mc->{EQUATION} .=  "\n;-->Outputs \n";
    foreach $name (@{$cfg->{outputs_index}}){
      $mc->{EQUATION} .= "$name ".&fetch_padding("$name ", $cfg->{species_length})."= ".&apply_format($cfg, $cfg->{outputs}->{$name}, 'monolix')."\n";
    }
    $mc->{OUTPUT}   .= "output = {".join(', ', @{$cfg->{outputs_index}})."} \n";
  }

  foreach $name      (keys(%{$mc})){
      $template =~ s#<$name>#$mc->{$name}#g;
  }

  open(FH, '>', &ftf($cfg, $cfg->{files}->{monolix}."-$parameter_set.txt"));
  print FH $template;
  close(FH);

}

sub dump_mrgsolve
{
  my ($cfg, $parameter_set) = @_;
  my $mc;
  my $text_string;
  my ($name, $name2);
  my ($parameter, $covariate);
  my $output;
  my $template = &fetch_template_file($cfg, 'mrgsolve.cpp');
  my ($counter, $counter2);

  my $namespace_map;

  my $iiv_set     = 'default';
  if($cfg->{iiv_index}->{$parameter_set}){
     $iiv_set = $parameter_set; 
  }

  my @cv_elements;
  my $cv_set     = 'default';



  $mc->{SYSTEM_PARAMS} = '';
  $mc->{STATE_INIT}    = '';
  $mc->{STATE_IC}      = '';
  $mc->{IIV_SP}        = '';
  $mc->{OMEGA}         = '';
  $mc->{SIGMA}         = '';
  $mc->{ODES}          = "\n//Defining the differential equations\n";
  $mc->{SSP}           = '';
  $mc->{DSP}           = '';
  $mc->{DSPINIT}       = '';
  $mc->{OUTPUTS}       = "\n";
  $mc->{OUTPUTSINIT}   = "";
  $mc->{CAPLIST}       = "";

  #
  # Dumping the system parameters
  #

  foreach $parameter (@{$cfg->{parameters_index}}){
    $name = $parameter;

    # Only processing system parameters
    if( $cfg->{parameters}->{$name}->{ptype} eq "system" ){

      # If the parameter has IIV we define the TV of that parameter
      if(defined($cfg->{iiv}->{$iiv_set}->{parameters}->{$name})){
        $mc->{SYSTEM_PARAMS} .= "TV_".$name;
        $mc->{SYSTEM_PARAMS} .= &fetch_padding("TV_".$name,20); 
        $mc->{SYSTEM_PARAMS} .= " : ";

        $text_string = &make_iiv($cfg, $name, 'C', $iiv_set) ;
        # substituting the placeholders
        $text_string =~ s#SIMINT_PARAMETER_TV#TV_$name#g;
        $text_string =~ s#SIMINT_IIV_VALUE#$cfg->{iiv}->{$iiv_set}->{parameters}->{$name}->{iiv_name}#g;

        $mc->{IIV_SP}        .= "double ".$name;
        $mc->{IIV_SP}        .= &fetch_padding("double $name", 30); 
        $mc->{IIV_SP}        .= " = ";
        $mc->{IIV_SP}        .= $text_string;
        $mc->{IIV_SP}        .= ";\n";
      }
      else{
        $mc->{SYSTEM_PARAMS} .= $name;
        $mc->{SYSTEM_PARAMS} .= &fetch_padding($name, 20); 
        $mc->{SYSTEM_PARAMS} .= " : ";
      }
      $mc->{SYSTEM_PARAMS} .= $cfg->{parameters}->{$name}->{value};
      $mc->{SYSTEM_PARAMS} .= &fetch_padding($cfg->{parameters}->{$name}->{value}, 10);
      $mc->{SYSTEM_PARAMS} .= " : ".$cfg->{parameters}->{$name}->{units};
      $mc->{SYSTEM_PARAMS} .= "\n";
    } elsif( $cfg->{parameters}->{$name}->{ptype} eq "variance" ){
      # Defining the sigma value
      if($mc->{SIGMA}  eq ""){
        $mc->{SIGMA} = '$SIGMA @annotated'."\n";
      }
      $mc->{SIGMA} .= $name.&fetch_padding($name, 10)." : ";
      $mc->{SIGMA} .= $cfg->{parameters}->{$name}->{value};
      $mc->{SIGMA} .= &fetch_padding($cfg->{parameters}->{$name}->{value}, 10);
      $mc->{SIGMA} .= " : ".$name."\n";
    }
  }

  if((@{$cfg->{covariates_index}})){
  foreach $covariate  (@{$cfg->{covariates_index}}){
    $name = $covariate; 

      if($cfg->{covariates}->{$name}->{parameter_sets}->{$parameter_set}){
        $cv_set = $parameter_set; 
      } else {
        $cv_set = "default";
      }

      # Pulling out the values of the covariate
      @cv_elements = &extract_elements($cfg->{covariates}->{$name}->{parameter_sets}->{$cv_set}->{values});
     
      $mc->{SYSTEM_PARAMS} .= $name;
      $mc->{SYSTEM_PARAMS} .= &fetch_padding($name, 20); 
      $mc->{SYSTEM_PARAMS} .= " : ";
      $mc->{SYSTEM_PARAMS} .= $cv_elements[0];
      $mc->{SYSTEM_PARAMS} .= &fetch_padding($cv_elements[0], 10);
      $mc->{SYSTEM_PARAMS} .= " : ".$cfg->{covariates}->{$name}->{values}->{units}." (covariate)";
      $mc->{SYSTEM_PARAMS} .= "\n";
    }
  }


  # 
  # Parsing the states
  # 
  foreach $name      (@{$cfg->{species_index}}){
    # initializing state names
    $mc->{STATE_INIT}    .= $name;
    $mc->{STATE_INIT}    .= &fetch_padding($name, 20); 
    $mc->{STATE_INIT}    .= " : -- \n";

    # Nonzero initial conditions
    if(defined($cfg->{initial_conditions}->{$name})){
      $text_string = $cfg->{initial_conditions}->{$name};
      $text_string = &apply_format($cfg, $text_string, 'C');

      $mc->{STATE_IC} .= $name."_0";
      $mc->{STATE_IC} .= &fetch_padding($name."_0", 20)." = ";
      $mc->{STATE_IC} .= $text_string;
      $mc->{STATE_IC} .= ";\n";
      }

    # Differential equations
    $mc->{ODES}          .= "dxdt_$name".&fetch_padding("dxdt_$name", 20)." = ".&make_ode($cfg, $name, 'C').";\n";
  }

  #
  # Secondary parameters
  #
  # Static secondary parameters 
  if ((@{$cfg->{static_secondary_parameters_index}})){
    foreach $parameter    (@{$cfg->{static_secondary_parameters_index}}){
      $name = $parameter;
      $mc->{SSP} .= "double ".$name;
      $mc->{SSP} .= &fetch_padding("double $name", 30)." = ";
      $mc->{SSP} .= &apply_format($cfg, $cfg->{static_secondary_parameters}->{$name}, 'C').";\n"; 
      if(defined($cfg->{if_conditional}->{$name})){
        $mc->{SSP} .= &extract_conditional($cfg, $name, 'C');
      }
    }
  }

  # Dynamic secondary parameters 
  if ((@{$cfg->{dynamic_secondary_parameters_index}})){
    $mc->{DSPINIT} .= "// Initialzing dynamic secondary parmaeters\n";
    $mc->{DSP}     .= "\n// Defining dynamic secondary parmaeters\n";
    foreach $parameter    (@{$cfg->{dynamic_secondary_parameters_index}}){
      $name = $parameter;
      # Initializing the dynamic secondary parameters
      $mc->{DSPINIT} .= "double ".$name;
      $mc->{DSPINIT} .= &fetch_padding("double $name", 30)." = 0.0; \n";

      # Defining dynamic secondary parameters
      $mc->{DSP} .= $name;
      $mc->{DSP} .= &fetch_padding($name, 30)." = ";
      $mc->{DSP} .= &apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$name}, 'C')."; \n"; 
      if(defined($cfg->{if_conditional}->{$name})){
        $mc->{DSP} .= &extract_conditional($cfg, $name, 'C');
      }
    }
  }

  #
  # Processing iiv information
  #
  if(keys(%{$cfg->{iiv}})){

    # mapping ETA()'s to IIV names and creating the header for the OMEGA block

    # creating the OMEGA BLOCK() line:
    $mc->{OMEGA} = '$OMEGA @annotated @block'."\n";

    # creating the  variance/covariance matrix
    $counter = 1;
    foreach $name (@{$cfg->{iiv_index}->{$iiv_set}}){
      $counter2 = 1;
      foreach $name2 (@{$cfg->{iiv_index}->{$iiv_set}}){
      if($counter2 le $counter){
        if($counter2 == 1){
           $mc->{OMEGA}       .= $name;
           $mc->{OMEGA}       .= &fetch_padding($name,10);
           $mc->{OMEGA}       .= " : ";
           }
        if(defined($cfg->{iiv}->{$iiv_set}->{vcv}->{$name}->{$name2})){
          $mc->{OMEGA}       .= $cfg->{iiv}->{$iiv_set}->{vcv}->{$name}->{$name2}.&fetch_padding($cfg->{iiv}->{$iiv_set}->{vcv}->{$name}->{$name2}, 10); }
        else{
          $mc->{OMEGA}       .= '0'.&fetch_padding('0', 10); }

        if($counter2 == $counter){
           $mc->{OMEGA}       .= &fetch_padding('',10)x(@{$cfg->{iiv_index}->{$iiv_set}} - $counter);
           $mc->{OMEGA}       .= " : ETA on  ".join(', ', @{$cfg->{iiv}->{$iiv_set}->{iivs}->{$name}->{parameters}})."\n";
           }
        $counter2 = $counter2 + 1; 
        }
      }
      $counter = $counter + 1;
    }
  }




  # Adding the outputs to the capture list
  $mc->{CAPLIST} .= join(' ', @{$cfg->{outputs_index}});

  $mc->{OUTPUTSINIT} = "// Initialzing model outputs\n";
  foreach $output    (@{$cfg->{outputs_index}}){
    $name = $output;
    $mc->{OUTPUTS}           .= $name.&fetch_padding($name, 30)." = ";
    $mc->{OUTPUTS}           .= &apply_format($cfg, $cfg->{outputs}->{$name}, 'C').";\n";

    $mc->{OUTPUTSINIT} .= "double ".$name;
    $mc->{OUTPUTSINIT} .= &fetch_padding("double $name", 30)." = 0.0; \n";

    if($cfg->{error}->{equations}->{$name}){

      # Initializing the output with the error
      $mc->{OUTPUTSINIT} .= "double SIDV_".$name;
      $mc->{OUTPUTSINIT} .= &fetch_padding("double SIDV_$name", 30)." = 0.0; \n";

      # Defining the output with error
      # pulling out the equation, swappign the actual name for PRED
      $text_string = $cfg->{error}->{equations}->{$name};
      my $namespace_map;
      $namespace_map->{PRED} = $name;
      $text_string = &remap_namespace($text_string, $namespace_map);
      $text_string = &apply_format($cfg, $text_string, 'C');
      $mc->{OUTPUTS} .= "SIDV_$name".&fetch_padding("SIDV_$name", 30)." = ".$text_string.";\n";
      
      # Adding it to the capture list
      $mc->{CAPLIST} .= " SIDV_$name";
    }
  }


  #
  # Mapping the content to the template file
  #
  foreach $name      (keys(%{$mc})){
    $template =~ s#<$name>#$mc->{$name}#g;
  }

  # Writing the template file
  open(FH, '>', &ftf($cfg, $cfg->{files}->{mrgsolve}."-$parameter_set.cpp"));
  print FH $template;
  close(FH);

}


sub dump_nonmem 
{
  my ($cfg, $parameter_set) = @_;
  my $species   = '';
  my $parameter = '';
  my $output    = '';
  my $name      = '';
  my $name2     = '';
  my $tmp_ode   = "";
  my $mc;
  my $counter;
  my $counter2;
  my $text_string;
  my $lower_bound = '';
  my $upper_bound = '';
  my $value       = '';

  my $iiv_set     = 'default';
  if($cfg->{iiv_index}->{$parameter_set}){
     $iiv_set = $parameter_set; 
  }


  # The names used in the error block of NONMEM the must be different from
  # those used in the DES block. However it may outputs can be defined in
  # terms of states and dynamic secondary parameters rename all of these
  # parameters in $ERROR. The namespace_map contains a mapping of these where
  # the hash keys are the origninal names 'name' and the values are the names 
  # to be used in $ERROR 'SIMINT_NMEB_name'
  my $namespace_map;
  $namespace_map->{SIMINT_TIME} = 'SIEB_TIME';

  my $template = &fetch_template_file($cfg, 'nonmem.ctl');
  # hash to hold the model components

  $mc->{INPUT}                             = '';
  $mc->{DATA}                              = '';
  $mc->{PARMAETER_VALUES}                  = '';
  $mc->{PARAMETERS_MAP}                    = '';
  $mc->{IIV_MAP}                           = '';
  $mc->{IIV_ON_PARAMETERS}                 = '';
  $mc->{STATIC_SECONDARY_PARAMETERS}       = '';
  $mc->{DYNAMIC_SECONDARY_PARAMETERS}      = '';
  $mc->{DYNAMIC_SECONDARY_PARAMETERS_NMEB} = '';
  $mc->{INITIAL_CONDITIONS}                = '';
  $mc->{COMP_ASSIGNMENT}                   = '';
  $mc->{STATES_ASSIGNMENT}                 = '';
  $mc->{STATES_ASSIGNMENT_NMEB}            = '';
  $mc->{ODES_ASSIGNMENT}                   = '';
  $mc->{ODES_MAPS}                         = '';
  $mc->{VARIANCE_ASSIGNMENT}               = '';
  $mc->{VARIANCE_PARAMETER_VALUES}         = '';

  $mc->{INFUSION_RATES}                    = '';




  #
  # Processing data info
  #

  if ($cfg->{data}->{file} ne ''){
    # defining the file name
    $mc->{DATA}   = $cfg->{data}->{file}." \n"; 
    # adding any ignore lines if any
    $mc->{DATA}  .= $cfg->{options}->{nonmem}->{data};
    # if we have header information then we put that 
    # into the INPUT block
    if((@{$cfg->{data}->{headers}->{values}})){
      $counter = 1;
      foreach $name (@{$cfg->{data}->{headers}->{values}}){
        #dropping and renameing the specified columns
        if(defined($cfg->{options}->{nonmem}->{input}->{drop}->{$name})){
          $name = $name."=DROP"; }
        elsif(defined($cfg->{options}->{nonmem}->{input}->{rename}->{$name})){
          $name = $cfg->{options}->{nonmem}->{input}->{rename}->{$name}; }

        if($counter < 6){
         $mc->{INPUT}       .= "$name ".&fetch_padding($name, $cfg->{parameters_length});
         $counter = $counter + 1;
        }
        else {
         $mc->{INPUT}       .= "$name ".&fetch_padding($name, $cfg->{parameters_length}).";\n";
         $counter = 1;
        }
      }
    }
    else{
     $mc->{INPUT} = "; No headers specified \n"; }
  }
  else{
     $mc->{INPUT} = "; No data file specified \n";
     $mc->{DATA}  = "; No data file specified \n"; }


  #
  # Processing system parameters
  #
  if ((@{$cfg->{parameters_system_index}})){
    $counter = 1;
    foreach $name (@{$cfg->{parameters_system_index}}){
      # mapping THETAs to actual names 
      if(defined($cfg->{iiv}->{$iiv_set}->{parameters}->{$name})){
        $mc->{PARAMETERS_MAP}       .= "SIMINT_TV_$name ".&fetch_padding("SIMINT_TV_$name", $cfg->{parameters_length})." = THETA($counter)\n";
        # pulling out the string defining the left hand side of the IIV term
        $text_string = &make_iiv($cfg, $name, 'nonmem', $iiv_set) ;
        # substituting the placeholders
        $text_string =~ s#SIMINT_PARAMETER_TV#SIMINT_TV_$name#g;
        $text_string =~ s#SIMINT_IIV_VALUE#$cfg->{iiv}->{$iiv_set}->{parameters}->{$name}->{iiv_name}#g;
        # constructing the definition
        $mc->{IIV_ON_PARAMETERS} .= "$name ".&fetch_padding("$name", $cfg->{parameters_length})." = $text_string\n";
      }
      else{
        $mc->{PARAMETERS_MAP}       .= "$name ".&fetch_padding($name, $cfg->{parameters_length})." = THETA($counter)\n";
      }
      # Dumping the values
      if(exists($cfg->{parameter_sets}->{$parameter_set}->{values}->{$name})){
        $value        = $cfg->{parameter_sets}->{$parameter_set}->{values}->{$name};}
      # otherwise we use the original value
      else{
        $value       = $cfg->{parameters}->{$name}->{value}; }
      $lower_bound =  $cfg->{parameters}->{$name}->{lower_bound};
      $upper_bound =  $cfg->{parameters}->{$name}->{upper_bound};
      # checking out the parameter bounds
      # if the bounds are equal then the parameter is Fixed
      $text_string = '';
      if($lower_bound eq $upper_bound){
        $text_string = "($value,".&fetch_padding($value,$cfg->{parameters_length})." FIX)  "; }
      else{
        # now we have a parameter with bounds
        # converting matlab specific values eps, inf
        #Lower bound
        if($lower_bound =~ m#eps#i ){
          $text_string = "(0.0,".&fetch_padding("0.0",$cfg->{parameters_length});} 
        elsif($lower_bound =~ m#-inf#i ){
          $text_string = "(-INF,".&fetch_padding("-INF",$cfg->{parameters_length});}
        else{
          $text_string = "($lower_bound,".&fetch_padding($lower_bound,$cfg->{parameters_length});}
     
        # Value
        $text_string .= $value.",".&fetch_padding($value,$cfg->{parameters_length});
     
        #upper bound
        if($upper_bound =~ m#inf#i ){
          $text_string .= "INF".&fetch_padding("INF",$cfg->{parameters_length}).")";}
        elsif($upper_bound =~ m#-eps#i ){
          $text_string .= "0.0".&fetch_padding("0.0",$cfg->{parameters_length}).")";} 
        else{
          $text_string .= "$upper_bound".&fetch_padding($upper_bound,$cfg->{parameters_length}).")";}
      }
      
      $mc->{PARAMETER_VALUES}    .= $text_string.&fetch_padding($text_string, 25);
      $mc->{PARAMETER_VALUES}    .= "; ".&fetch_padding($counter, 2)."$counter";  
      $mc->{PARAMETER_VALUES}    .= " $name ".&fetch_padding($name, $cfg->{parameters_length});
      $mc->{PARAMETER_VALUES}    .= " $cfg->{parameters}->{$name}->{units} \n";
      $counter = $counter + 1;
    }
  }

  #
  # Processing infusion rate information
  #
  if((@{$cfg->{input_rates_index}})){
    $mc->{INFUSION_RATES} .= "\n; Infusion rates should be specified in the dataset\n";
    $mc->{INFUSION_RATES} .= "; placeholders for other targets will be disabled  \n";
    $mc->{INFUSION_RATES} .= "; by setting those values to zero here             \n";
    foreach $name (@{$cfg->{input_rates_index}}){
      $mc->{INFUSION_RATES} .= $name.&fetch_padding($name, $cfg->{inputs_length})."= 0.0\n"
    }
  }
  
  #
  # Processing iiv information
  #
  if(keys(%{$cfg->{iiv}})){

    # mapping ETA()'s to IIV names and creating the header for the OMEGA block
    $mc->{IIV_MAP} = "; Defining the iiv variables\n";
    $counter = 1;
    foreach $name (@{$cfg->{iiv_index}->{$iiv_set}}){
      $mc->{IIV_MAP}       .= "$name ".&fetch_padding($name, $cfg->{parameters_length})." = ETA($counter)\n";
      $counter = $counter+1; }

    # creating the OMEGA BLOCK() line:
    $mc->{IIV_VALUES} = '$OMEGA BLOCK('.scalar(@{$cfg->{iiv_index}->{$iiv_set}}).")\n"; 

    # creating the  variance/covariance matrix
    $counter = 1;

    foreach $name (@{$cfg->{iiv_index}->{$iiv_set}}){
      $counter2 = 1;
      foreach $name2 (@{$cfg->{iiv_index}->{$iiv_set}}){
      if($counter2 le $counter){
        if(defined($cfg->{iiv}->{$iiv_set}->{vcv}->{$name}->{$name2})){
          $mc->{IIV_VALUES}       .= $cfg->{iiv}->{$iiv_set}->{vcv}->{$name}->{$name2}.&fetch_padding($cfg->{iiv}->{$iiv_set}->{vcv}->{$name}->{$name2}, $cfg->{parameters_length}); }
        else{
          $mc->{IIV_VALUES}       .= '0'.&fetch_padding('0', $cfg->{parameters_length}); }



          if($counter2 == $counter){
             $mc->{IIV_VALUES}       .= &fetch_padding('',$cfg->{parameters_length})x(@{$cfg->{iiv_index}->{$iiv_set}} - $counter);
             $mc->{IIV_VALUES}       .= "; $name\n"; }
          


       #if($name eq $name2){
       #  $mc->{IIV_VALUES}       .= "; Var: $name\n"; }
       #else{
       #  $mc->{IIV_VALUES}       .= "; Cov: $name-$name2\n"; }
        $counter2 = $counter2 + 1; 
        }
      }
      $counter = $counter + 1;
    }
  }
  else{
      $mc->{IIV_VALUES} .= "; No IIV parameters defined\n"; 
      $mc->{IIV_VALUES} .= "; See: <IIV:?> and <IIVCOR:?> \n"; }

  #
  # Processing variance parameters
  #
  if ((@{$cfg->{parameters_variance_index}})){
    $counter = 1;
    foreach $name (@{$cfg->{parameters_variance_index}}){
      $mc->{VARIANCE_ASSIGNMENT} .= "$name".&fetch_padding($name,$cfg->{parameters_length})." = SIGMA($counter)\n";
      $mc->{VARIANCE_PARAMETER_VALUES} .= $cfg->{parameters}->{$name}->{value};
      $mc->{VARIANCE_PARAMETER_VALUES} .= &fetch_padding($cfg->{parameters}->{$name}->{value}, $cfg->{parameters_length});
      $mc->{VARIANCE_PARAMETER_VALUES} .= "; $name ".&fetch_padding($name, $cfg->{parameters_length});
      $mc->{VARIANCE_PARAMETER_VALUES} .= " $cfg->{parameters}->{$name}->{units} \n";
      $counter = $counter + 1;
    }
  }
  else{
      $mc->{VARIANCE_PARAMETER_VALUES} .= "; No variance parameters defined\n"; 
      $mc->{VARIANCE_PARAMETER_VALUES} .= "; See: <VP> \n"; }

  #
  # Processing static secondary parameters
  #
  if ((@{$cfg->{static_secondary_parameters_index}})){
    foreach $name (@{$cfg->{static_secondary_parameters_index}}){
      $text_string = $cfg->{static_secondary_parameters}->{$name};
      $text_string = &apply_format($cfg, $text_string, 'nonmem');
      $mc->{STATIC_SECONDARY_PARAMETERS}     .= "$name ".&fetch_padding($name, $cfg->{parameters_length})." = $text_string\n";
    }
  }

  #
  # Dynamic secondary parameters
  #
  if ((@{$cfg->{dynamic_secondary_parameters_index}})){
    foreach $name (@{$cfg->{dynamic_secondary_parameters_index}}){
      $value  = $cfg->{dynamic_secondary_parameters}->{$name};
      $value  = &apply_format($cfg, $value, 'nonmem');
      $mc->{DYNAMIC_SECONDARY_PARAMETERS}  .= $name.&fetch_padding($name, $cfg->{parameters_length})." = ".$value."\n";

      if(defined($cfg->{if_conditional}->{$name})){
        $mc->{DYNAMIC_SECONDARY_PARAMETERS} .= &extract_conditional($cfg, $name, 'nonmem');
      }

      # appending the state naming information in to the namespace map
      $namespace_map->{$name} = "SIEB_$name";
    }
  }

  #
  # states and ODES
  #
  if ((@{$cfg->{species_index}})){
    $counter = 1;

    foreach $name (@{$cfg->{species_index}}){
      # name2 will store the state name if no amtify has been defined,
      # otherwise it will store the amtify name 
      # defining the states themselves
      # if the current state has been amtified we stick that into the amtify map
      $name2 = $name;
      if(defined($cfg->{options}->{amtify}->{cmt_to_amt}->{$name})){ 
        $name2 = $cfg->{options}->{amtify}->{cmt_to_amt}->{$name};
      }
      $mc->{COMP_ASSIGNMENT} .= "COMP=($name2) ".&fetch_padding("COMP=($name2) ", 30)."; # $counter \n"; 
     
      $mc->{STATES_ASSIGNMENT} .= "$name2".&fetch_padding("$name", $cfg->{species_length})."=  A($counter) \n";

      # appending the state naming information in to the namespace map
      $namespace_map->{$name2} = "SIEB_$name2";

      # Dumping the initial conditions here
      if(defined($cfg->{initial_conditions}->{$name})){
        $text_string = $cfg->{initial_conditions}->{$name};
        $text_string = &apply_format($cfg, $text_string, 'nonmem');

        $mc->{INITIAL_CONDITIONS} .= "A_0($counter) = $text_string ".&fetch_padding($text_string, $cfg->{species_length})."; $name2 \n" ;}
      else{
        $mc->{INITIAL_CONDITIONS} .= "A_0($counter) = 0.0 ".&fetch_padding("0.0", $cfg->{species_length})."; $name2 \n" ;}

      # defining the ODES a human readable format
      $mc->{ODES_ASSIGNMENT}   .= "SIMINT_d$name2 ".&fetch_padding($name, $cfg->{species_length})." = ";
      $mc->{ODES_ASSIGNMENT}   .= &make_ode($cfg, $name, 'nonmem');
      $mc->{ODES_ASSIGNMENT}   .= "\n";

      # mapping those odes back to their DADT counterparts
      $mc->{ODES_MAP}          .= "DADT($counter) = SIMINT_d$name2 \n" ;




    $counter = $counter + 1;
    }


  }

  # Remapping the state and dynamic secondary parameter definitions 
  # to be used in the $ERROR block
  $mc->{STATES_ASSIGNMENT_NMEB} = $mc->{STATES_ASSIGNMENT};
  $mc->{STATES_ASSIGNMENT_NMEB} = &remap_namespace($mc->{STATES_ASSIGNMENT_NMEB}, $namespace_map);

  $mc->{DYNAMIC_SECONDARY_PARAMETERS_NMEB} = $mc->{DYNAMIC_SECONDARY_PARAMETERS};
  $mc->{DYNAMIC_SECONDARY_PARAMETERS_NMEB} = &remap_namespace($mc->{DYNAMIC_SECONDARY_PARAMETERS_NMEB}, $namespace_map);


  foreach $name      (keys(%{$mc})){
    $template =~ s#<$name>#$mc->{$name}#g;
  }

  open(FH, '>', &ftf($cfg, $cfg->{files}->{nonmem}."-$parameter_set.ctl"));
  print FH $template;
  close(FH);

}

#---------------------------------------------------------------------------
# Takes a string containing one or more expressions and replaces the named
# elements found as keys in mymap with the values associated with those keys.
#---------------------------------------------------------------------------
sub remap_namespace{
  my ($string, $mymap) = @_;

  my $name;
  
  if(keys %{$mymap}){
    # wrapping spacer strings around spaces to 
    # preserve the spacing in the original 
    $string =~ s# #%%SPACER%% %%SPACER%%#g;
    # placing spacers on either end of the lines
    $string =~ s#^#%%SPACER%%#gm;
    $string =~ s#$#%%SPACER%%#gm;

    # also placing spacers around parentheses and 
    # between brackets and what they are holding:
    $string =~ s#\[#\[%%SPACER%%#gm;
    $string =~ s#\]#%%SPACER%%\]#gm;
    $string =~ s#\(#%%SPACER%%\(%%SPACER%%#gm;
    $string =~ s#\)#%%SPACER%%\)%%SPACER%%#gm;

    # wrapping spacers around operators
    $string =~ s#=#%%SPACER%%=%%SPACER%%#g;
    $string =~ s#\*#%%SPACER%%\*%%SPACER%%#g;
    $string =~ s#\/#%%SPACER%%\/%%SPACER%%#g;
    $string =~ s#-#%%SPACER%%-%%SPACER%%#g;
    $string =~ s#\+#%%SPACER%%\+%%SPACER%%#g;
    $string =~ s#;#%%SPACER%%;%%SPACER%%#g;

    # now all named species should be surrounded by spacers
    # and we can then do exact matching of the name with the 
    # spacers  on either side.
    # remapping names:
    foreach $name (keys(%{$mymap})){
      $string =~ s#%%SPACER%%$name%%SPACER%%#%%SPACER%%$mymap->{$name}%%SPACER%%#g;
    }
  }

  # pulling out the spacers
  $string =~ s#%%SPACER%% %%SPACER%%# #g;
  $string =~ s#%%SPACER%%##g;
  return $string;
}

sub dump_berkeley_madonna
{
  my ($cfg, $parameter_set) = @_;

  my $species   = '';
  my $parameter = '';
  my $output    = '';
  my $name      = '';
  my $cvpset    = '';

  my $tmp_text  = '';

  my $cv_namespace_map;

 #my $name      = '';
 #my $term      = '';
 #my $counter   = 0;


  my $bmfile_output  = '';


  #$bmfile_output .= "; Autogenerated Berkeley Madonna Target\n";
  $bmfile_output .= &fetch_comments($cfg->{comments}, 'bm');
  $bmfile_output .= "METHOD Stiff\n";
  $bmfile_output .= "RENAME TIME = SIMINT_TIME\n";






  $bmfile_output .= "\n\n\n{INITIAL VALUES}\n";
  foreach $species (@{$cfg->{species_index}}){
    if(exists($cfg->{initial_conditions}->{$species})){
      $bmfile_output .= "INIT $species".&fetch_padding($species, $cfg->{species_length})." = ".&apply_format($cfg, $cfg->{initial_conditions}->{$species}, 'bm')."\n";     
    }
    else{
      $bmfile_output .= "INIT $species".&fetch_padding($species, $cfg->{species_length})." = 0.0 \n";     
    }
  }

  
  $bmfile_output .= "\n\n\n{PARAMETERS}\n";


  # parameter initializing 
  if ((@{$cfg->{parameters_index}})){
    $bmfile_output .= "; parameters\n";
    foreach $parameter (@{$cfg->{parameters_index}}){
      if(exists($cfg->{parameter_sets}->{$parameter_set}->{values}->{$parameter})){
        $bmfile_output .=  "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})." = ".$cfg->{parameter_sets}->{$parameter_set}->{values}->{$parameter}." ";
        $bmfile_output .=  " ".&fetch_padding($cfg->{parameter_sets}->{$parameter_set}->{values}->{$parameter}, $cfg->{parameters_length})." ; ".$cfg->{parameters}->{$parameter}->{units}."\n";}
      else{
        $bmfile_output .=  "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})." = ".$cfg->{parameters}->{$parameter}->{value}." ";
        $bmfile_output .=  " ".&fetch_padding($cfg->{parameters}->{$parameter}->{value}, $cfg->{parameters_length})." ; ".$cfg->{parameters}->{$parameter}->{units}."\n";}
    }
  }

  if((@{$cfg->{covariates_index}})){
    $bmfile_output .= "\n;---------------------------------------- \n";
    $bmfile_output .= "; Placeholder for covariates             \n";
    $bmfile_output .= "; You need to edit this to reflect the   \n";
    $bmfile_output .= "; covariates you're trying to test       \n";
    $bmfile_output .= "; The prefix SIMINT_CVCIC indicates the  \n";
    $bmfile_output .= "; inital value of the covariate which    \n";
    $bmfile_output .= "; can be used in defining static         \n";
    $bmfile_output .= "; secondary parameters  \n \n";

    # fit the covariate hasn't been specified for this parameter set 
    # then we revert to the default
    foreach $name (@{$cfg->{covariates_index}}){
      if(defined($cfg->{covariates}->{$name}->{parameter_sets}->{$parameter_set})){
        $cvpset = $parameter_set; }
      else{
        $cvpset = 'default'; }
      $bmfile_output .="; covariate: $name \n";
      $bmfile_output .="; type:      ".$cfg->{covariates}->{$name}->{cv_type}."\n";
      $bmfile_output .="; times:     ".$cfg->{covariates}->{$name}->{parameter_sets}->{$cvpset}->{times};
      $bmfile_output .=         "  (".$cfg->{covariates}->{$name}->{times}->{units}.")  \n";
      $bmfile_output .="; values:    ".$cfg->{covariates}->{$name}->{parameter_sets}->{$cvpset}->{values};
      $bmfile_output .=         "  (".$cfg->{covariates}->{$name}->{values}->{units}.") \n";
      $bmfile_output .= $name.&fetch_padding($name, $cfg->{inputs_length})."= 0.0". ";\n";
      $bmfile_output .= "SIMINT_CVIC_$name".&fetch_padding("SIMINT_CVIC_$name", $cfg->{inputs_length})."= 0.0". ";\n\n";
      $cv_namespace_map->{$name} = "SIMINT_CVIC_$name";
    }
    $bmfile_output .= ";---------------------------------------- \n";
  }




  # static secondary parameter initializing 
  if ((@{$cfg->{static_secondary_parameters_index}})){
    $bmfile_output .= "\n\n; static secondary parameters\n";
    foreach $parameter (@{$cfg->{static_secondary_parameters_index}}){
      if(defined($cfg->{if_conditional}->{$parameter})){
        $tmp_text =  "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})." = ".&extract_conditional($cfg, $parameter, 'bm')."\n";
        }
      else{
        $tmp_text =  "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})." = ".&apply_format($cfg, $cfg->{static_secondary_parameters}->{$parameter}, 'bm')." \n";     
        }
 
        $bmfile_output .= &remap_namespace($tmp_text, $cv_namespace_map); 
    }
  }


  if((@{$cfg->{input_rates_index}})){
    $bmfile_output .= "\n\n; Placeholder for infuion rates \n";
    $bmfile_output .= "; You need to edit this to reflect the \n";
    $bmfile_output .= "; the infusions for this system \n";
    foreach $name (@{$cfg->{input_rates_index}}){
      $bmfile_output .="; time scale: ".$cfg->{input_rates}->{$name}->{times}->{scale}. " units: (".$cfg->{input_rates}->{$name}->{times}->{units}.") \n";
      $bmfile_output .="; mass scale: ".$cfg->{input_rates}->{$name}->{levels}->{scale}." units: (".$cfg->{input_rates}->{$name}->{levels}->{units}.") \n"; ;
      $bmfile_output .= $name.&fetch_padding($name, $cfg->{inputs_length})."= 0.0". ";\n"
    }
  }

  # static secondary parameter initializing 
  if ((@{$cfg->{dynamic_secondary_parameters_index}})){
    $bmfile_output .= "\n\n; dynamic secondary parameters\n";
    foreach $parameter (@{$cfg->{dynamic_secondary_parameters_index}}){
      if(defined($cfg->{if_conditional}->{$parameter})){
        # if the parameter has an if/then associated with it then we 
        # dump it according to the if/then definition
        $bmfile_output .=  "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})." = ".&extract_conditional($cfg, $parameter, 'bm')."\n";
      }
      else{
        #otherwise we use the value specified in the <Ad> call
        $bmfile_output .=  "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})." = ".&apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$parameter}, 'bm')." \n";     
      }
    }
  }



  $bmfile_output .= "\n\n\n{DIFFERENTIAL EQUATIONS}\n";

  foreach $species (@{$cfg->{species_index}}){
    $bmfile_output .=   "d/dt($species)".&fetch_padding($species, ($cfg->{species_length}))." = ";     
    if((@{$cfg->{species}->{$species}->{production}})){
    $bmfile_output .=   &apply_format($cfg, join(' + ', @{$cfg->{species}->{$species}->{production}}), 'bm');
     }
    if((@{$cfg->{species}->{$species}->{consumption}})){
    $bmfile_output .=   " - ";
    $bmfile_output .=   &apply_format($cfg, join(' - ', @{$cfg->{species}->{$species}->{consumption}}), 'bm');
    }
   if((@{$cfg->{species}->{$species}->{odes}})){
     if((scalar(@{$cfg->{species}->{$species}->{production}}) > 0) or 
        (scalar(@{$cfg->{species}->{$species}->{consumption}}) > 0)){
        $bmfile_output .=   " + ";
      }
      $bmfile_output .=   &apply_format($cfg, join(' + ', @{$cfg->{species}->{$species}->{odes}}), 'bm');
    }
    $bmfile_output .=   " \n";
  }

  $bmfile_output .= "\n\n\n{OUTPUTS}\n";


 if((@{$cfg->{outputs_index}})){
   foreach $output (@{$cfg->{outputs_index}}){
     $bmfile_output .=   $output.&fetch_padding($output, ($cfg->{outputs_length}))." = ".&apply_format($cfg, $cfg->{outputs}->{$output}, 'bm')."\n";     

   }
  }
  

  open(FH, '>', &ftf($cfg, $cfg->{files}->{berkeley_madonna}."-$parameter_set.txt"));
  print FH $bmfile_output;
  close(FH);

}



sub dump_matlab
{
  my ($cfg) = @_;

  my $species   = '';
  my $name      = '';
  my $name2     = '';
  my $set_id    = '';
  my $output    = '';
  my $parameter = '';
  my $term      = '';
  my $counter   = 0;
  my $counter2  = 0;

  my $field; 

  my $production_rates; 
  my $consumption_rates;

  
  my $ae;
  my $ae_template =  &fetch_matlab_auto_analysis_estimation();
  $ae->{PSETS}                 = '';
  $ae->{BOLUS}                 = ''; 
  $ae->{INFUSION_RATES}        = ''; 
  $ae->{COVARIATES}            = ''; 
  $ae->{OUTPUT_TIMES}          = ''; 


  # variables to hold components of the system
  # to be placed in m-files
  my $m_common_block = '';
  my $m_odes         = '';
  my $m_outputs      = '';
  my $cv_namespace_map = {};

  my $tmp_text = '';

  # Things to check for:
  # rates have both times and levels
  my $tmp_file_chunk = '';

    open(FHCOMMON_BLOCK,          '>', &ftf($cfg, $cfg->{files}->{common_block}));

    open(FHINITIALIZE,            '>', &ftf($cfg, $cfg->{files}->{initialize}));
    open(FHODES,                  '>', &ftf($cfg, $cfg->{files}->{odes}));
    open(FHREMAP_ODES,            '>', &ftf($cfg, $cfg->{files}->{remap_odes}));
    open(FHOUTPUTS,               '>', &ftf($cfg, $cfg->{files}->{outputs}));

    open(FHODESM,                 '>', &ftf($cfg, $cfg->{files}->{odes_m}));
    open(FHSIMM,                  '>', &ftf($cfg, $cfg->{files}->{sim_m}));

    #open(FHMAP_INDICES,           '>',&ftf($cfg,  $cfg->{files}->{map_indices});
    #open(FHFETCH_PARAMETERS,      '>',&ftf($cfg,  $cfg->{files}->{fetch_parameters});

    open(FHFETCH_SYSINFO,         '>', &ftf($cfg, $cfg->{files}->{fetch_system_information}));

    open(FHSIM_DRIVER,            '>', &ftf($cfg, $cfg->{files}->{simulation_driver}));
    open(FHMAP_SIMULATION_OUTPUT, '>', &ftf($cfg, $cfg->{files}->{map_simulation_output}));


    #
    # Dumping C stuff
    #

    #
    # initializing all of the C variables
    #
    # Parameters
    $counter = 0;

    print FHCOMMON_BLOCK        "/* Internal Variables */\n"; 
    print FHCOMMON_BLOCK        "double SIMINT_TIME = 0.0;\n";

    print FHCOMMON_BLOCK        "/* Initializing parameters */\n"; 
    foreach $parameter (@{$cfg->{parameters_index}}){
      print FHCOMMON_BLOCK      "double $parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= 0.0; \n";     
      $counter = $counter+1;
    }

    # Infusion rates
    if((@{$cfg->{input_rates_index}})){
      print FHCOMMON_BLOCK        "\n\n\n/* Initializing infusion rates */\n"; 
      foreach $name (@{$cfg->{input_rates_index}}){
      print FHCOMMON_BLOCK      "double $name".&fetch_padding($name, $cfg->{parameters_length})."= 0.0; \n";     
      $counter = $counter + 1;
      }
    }

    # Covariates     
    if((@{$cfg->{covariates_index}}) ){
      print FHCOMMON_BLOCK        "\n\n\n/* Initializing covariates*/\n"; 
      foreach $name (@{$cfg->{covariates_index}}){
      print FHCOMMON_BLOCK      "double $name".&fetch_padding($name, $cfg->{parameters_length})."= 0.0; \n";     
      $counter = $counter + 1;
      print FHCOMMON_BLOCK      "double SIMINT_CVIC_$name".&fetch_padding("SIMINT_CVIC_$name", $cfg->{parameters_length})."= 0.0; \n";     
      $counter = $counter + 1;

      # storing the mapping information in the namespace map
      $cv_namespace_map->{$name} = "SIMINT_CVIC_$name";
      }
    }

    # derivatives dstate = 0
    print FHCOMMON_BLOCK        "\n\n\n/* Initializing derivatives  */\n"; 
    foreach $species (@{$cfg->{species_index}}){
      print FHCOMMON_BLOCK    "double SIMINT_d$species".&fetch_padding("SIMINT_$species", $cfg->{species_length})."= 0.0; \n";     
    }

    # static secondary parameter initializing 
    if ((@{$cfg->{static_secondary_parameters_index}})){
      print FHCOMMON_BLOCK        "\n\n\n /* Initializing dynamic secondary paramters */\n"; 
      foreach $parameter (@{$cfg->{static_secondary_parameters_index}}){
          print FHCOMMON_BLOCK          "double $parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= 0.0; \n";     
      }
    }

    # states state = x[n];
    $counter = 0;
    print FHCOMMON_BLOCK        "\n\n\n/* Mapping states to their common names */\n"; 
    foreach $species (@{$cfg->{species_index}}){
      print FHCOMMON_BLOCK    "double $species".&fetch_padding($species, $cfg->{species_length})." = 0.0;\n";     
      $counter = $counter+1;
    }

    # dynamic secondary parameter initializing 
    if (@{$cfg->{dynamic_secondary_parameters_index}}){
      print FHCOMMON_BLOCK        "\n\n\n /* Defining dynamic secondary paramters */\n"; 
      foreach $parameter (@{$cfg->{dynamic_secondary_parameters_index}}){
          print FHCOMMON_BLOCK          "double $parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= 0.0;\n";     
      }
    }

    print FHCOMMON_BLOCK        "\n\n\n/* Defining internal values */\n";
    print FHCOMMON_BLOCK        "SIMINT_TIME = ssGetT(S);\n";

    print FHCOMMON_BLOCK        "\n\n\n/* \n * Mapping C inputs to variable names and \n * defining secondary parameters  \n */\n\n\n"; 

    #
    # Mapping Inputs to variable names and defining
    # the relationships for parameters
    #
     
    # for the m-file common block, I'm sampling the input vector at the current time
     
    $m_common_block .= "% pulling out the inputs at the current simulation time\n";
    $m_common_block .= "u = interp1(S.blocks.all_inputs(:,1), S.blocks.all_inputs, SIMINT_TIME);\n";
    $m_common_block .= "% The first column is the interpolated time,\n";
    $m_common_block .= "% so we remove that one.\n";
    $m_common_block .= "u = u(1,2:end);\n";

    # parameter initializing 
    $counter = 0;
    print FHCOMMON_BLOCK        "/* Defining parameters */\n"; 
    $m_common_block .= "% Defining parameters \n";
    foreach $parameter (@{$cfg->{parameters_index}}){
      print FHCOMMON_BLOCK      "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= u[$counter]; \n";     

      $m_common_block .=        "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= u(".($counter+1)."); \n";     
      $counter = $counter+1;
    }


    # Infusion rates
    if(keys %{$cfg->{input_rates}}){
      print FHCOMMON_BLOCK        "\n\n\n/* Defining infusion rates */\n"; 
      $m_common_block .= "\n\n\n% Defining infusion rates \n";
      foreach $name (@{$cfg->{input_rates_index}}){
        print FHCOMMON_BLOCK      "$name".&fetch_padding($name, $cfg->{parameters_length})."= u[$counter]; \n";     
        $m_common_block .=        "$name".&fetch_padding($name, $cfg->{parameters_length})."= u(".($counter+1)."); \n";     
      $counter = $counter + 1;
      }
    }
     
    # Covariates          
    if(keys %{$cfg->{covariates}}){
      print FHCOMMON_BLOCK        "\n\n\n/* Defining covariates */\n"; 
      $m_common_block .= "\n\n\n% Defining covariates \n";
      foreach $name (@{$cfg->{covariates_index}}){
      # This dumps the timeseries
      print FHCOMMON_BLOCK      "$name".&fetch_padding($name, $cfg->{parameters_length})."= u[$counter]; \n";     
      $m_common_block .=        "$name".&fetch_padding($name, $cfg->{parameters_length})."= u(".($counter+1)."); \n";     
      $counter = $counter + 1;
      # This dumps the initial condition
      print FHCOMMON_BLOCK      "SIMINT_CVIC_$name".&fetch_padding("SIMINT_CVIC_$name", $cfg->{parameters_length})."= u[$counter]; \n";     
      $m_common_block .=        "SIMINT_CVIC_$name".&fetch_padding("SIMINT_CVIC_$name", $cfg->{parameters_length})."= u(".($counter+1)."); \n";     
      $counter = $counter + 1;
      }
    }

    # static secondary parameter initializing 
    if ((@{$cfg->{static_secondary_parameters_index}})){
      print FHCOMMON_BLOCK        "\n\n\n /* Defining static secondary paramters */\n"; 
      print FHCOMMON_BLOCK &extract_conditional($cfg, 'not defined', 'C');
      $m_common_block .= "\n\n\n% Defining static secondary parameters\n";
      foreach $parameter (@{$cfg->{static_secondary_parameters_index}}){
          # we have to remap the covariate parameters so they take on the
          # initial condition (SIMINT_CVIC_CVNAME) instead of the timeseries
          # CVNAME
          $tmp_text  = "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."=".&apply_format($cfg, $cfg->{static_secondary_parameters}->{$parameter}, 'C')."; \n";     
          $tmp_text .= &extract_conditional($cfg, $parameter, 'C');
          print FHCOMMON_BLOCK &remap_namespace($tmp_text, $cv_namespace_map);
          $tmp_text  = "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."=".&apply_format($cfg, $cfg->{static_secondary_parameters}->{$parameter}, 'matlab')."; \n";     
          $tmp_text .= &extract_conditional($cfg, $parameter, 'matlab')."\n";
          $m_common_block  .=   &remap_namespace($tmp_text, $cv_namespace_map); 
      }
    }

    # states state = x[n];
    $counter = 0;
    print FHCOMMON_BLOCK        "\n\n\n/* Mapping states to their common names */\n"; 
    $m_common_block .= "\n\n\n%  Mapping states to their common names \n";
    foreach $species (@{$cfg->{species_index}}){
      print FHCOMMON_BLOCK    "$species".&fetch_padding($species, $cfg->{species_length})." = x[".int($counter)."];\n";     
      $m_common_block .=      "$species".&fetch_padding($species, $cfg->{species_length})." = x(".int($counter+1).");\n";     
      $counter = $counter+1;
    }

    # dynamic secondary parameter initializing 
    if ((@{$cfg->{dynamic_secondary_parameters_index}})){
      print FHCOMMON_BLOCK        "\n\n\n /* Defining dynamic secondary paramters */\n"; 
      print FHCOMMON_BLOCK &extract_conditional($cfg, 'not defined', 'C');
      $m_common_block .=    "\n\n\n% Defining dynamic secondary paramters \n"; 
      $m_common_block .=  &extract_conditional($cfg, 'not defined', 'matlab')."\n";

      foreach $parameter (@{$cfg->{dynamic_secondary_parameters_index}}){
          print FHCOMMON_BLOCK          "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= ".&apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$parameter}, 'C').";\n";     
          print FHCOMMON_BLOCK &extract_conditional($cfg, $parameter, 'C');

          $m_common_block .=  "$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= ".&apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$parameter}, 'matlab').";\n";     
          $m_common_block .=  &extract_conditional($cfg, $parameter, 'matlab')."\n";

      }
    }
  
    # dumping odes to a file
    foreach $species (@{$cfg->{species_index}}){

      # padding production and consumption terms to make the output pretty
      @{$production_rates}  = ();
      @{$consumption_rates} = ();
      $counter = 0;
      foreach $term (@{$cfg->{species}->{$species}->{production}}){
        #$cfg->{species}->{$species}->{production}->[$counter] = $term.&fetch_padding($term, $cfg->{term_length});
        push @{$production_rates}, $term.&fetch_padding($term, $cfg->{term_length}); 
        $counter = $counter+1;
      }

      $counter = 0;
      foreach $term (@{$cfg->{species}->{$species}->{consumption}}){
        #$cfg->{species}->{$species}->{consumption}->[$counter] = $term.&fetch_padding($term, $cfg->{term_length});
        push @{$consumption_rates}, $term.&fetch_padding($term, $cfg->{term_length}); 
        $counter = $counter+1;
      }

      # This contains the left-hand-side of the
      # ODE definitions
      print FHODES "SIMINT_d$species".&fetch_padding("SIMINT_$species", ($cfg->{species_length}))." = ";     
      $m_odes .=   "SIMINT_d$species".&fetch_padding("SIMINT_$species", ($cfg->{species_length}))." = ";     


      #
      # only dumping production terms if they exist
      #
      if((@{$production_rates})){
        print FHODES &apply_format($cfg, join(' + ', @{$production_rates}), 'C');
        $m_odes .=   &apply_format($cfg, join(' + ', @{$production_rates}), 'matlab');
      }

      #
      # only dumping consumption terms if they exist
      #
      if((@{$consumption_rates})){
        # if there were production terms, we wrap to the next line
        if(scalar(@{$production_rates}) > 0){
             print FHODES "\n".&fetch_padding('', ($cfg->{species_length}+1));
        $m_odes .=   " ... \n".&fetch_padding('', ($cfg->{species_length}+1));
        }
        print FHODES " - ";
        print FHODES &apply_format($cfg, join(' - ', @{$consumption_rates}), 'C');
        $m_odes .=   " - ";
        $m_odes .=   &apply_format($cfg, join(' - ', @{$consumption_rates}), 'matlab');
      }

      #
      # only dumping the ode terms if they exist
      #
      if((@{$cfg->{species}->{$species}->{odes}})){

        # wrapping to the next line if production or consumption terms have been specified
        if((scalar(@{$cfg->{species}->{$species}->{production}}) > 0) or 
           (scalar(@{$cfg->{species}->{$species}->{consumption}}) > 0)){
                print FHODES "\n".&fetch_padding('+ ', ($cfg->{species_length}+1))."+ ";
           $m_odes .=   " ... \n".&fetch_padding('+ ', ($cfg->{species_length}+1))."+ ";
         }
         print FHODES &apply_format($cfg, join(' + ', @{$cfg->{species}->{$species}->{odes}}), 'C');
         $m_odes .=   &apply_format($cfg, join(' + ', @{$cfg->{species}->{$species}->{odes}}), 'matlab');
       }

      # ending the definition
      print FHODES ";\n";
      $m_odes .=   ";\n";
    }

    # dumping map back to C variables
    $counter = 0;
    $m_odes .=   "\n\n%remapping the ODEs back to dx\n";
    foreach $species (@{$cfg->{species_index}}){
      print FHREMAP_ODES "dx[".int($counter)."] = SIMINT_d$species; \n";     
      $m_odes .=         "dx(".int($counter+1).",1) = SIMINT_d$species; \n";     
      $counter = $counter+1;
    }

    # dumping output assignments
    $counter = 0;
    foreach $output (@{$cfg->{outputs_index}}){
      print FHOUTPUTS    " y[".int($counter  )."] =".&apply_format($cfg, $cfg->{outputs}->{$output}, 'C').";\n";     
      $m_outputs .=      " y(SIMINT_TIMEIDX,".int($counter+1).") = ".&apply_format($cfg, $cfg->{outputs}->{$output}, 'matlab').";\n";     
      $counter = $counter+1;
    }


    # dumping size information
    # number of parameters, states, and outputs
    # to a file
$tmp_file_chunk = "
ssSetNumContStates(    S, ".scalar((@{$cfg->{species_index}})).");  
ssSetNumInputs(        S, ".(scalar((@{$cfg->{parameters_index}})) + scalar((keys(%{$cfg->{input_rates}})))+ 2*scalar((@{$cfg->{covariates_index}}))) .");  
ssSetNumOutputs(       S, ".scalar((@{$cfg->{outputs_index}})).");  
ssSetNumDiscStates(    S, 0);
ssSetDirectFeedThrough(S, 1);
ssSetNumSampleTimes(   S, 1);
ssSetNumSFcnParams(    S, 0);
ssSetNumRWork(         S, 0);
ssSetNumIWork(         S, 0);
ssSetNumPWork(         S, 0);
";

    print FHINITIALIZE $tmp_file_chunk;


    #
    # Dumping matlab files
    #


    #
    # Creating parameter information
    # 

    $tmp_file_chunk  = "function [cfg]=auto_fetch_system_information()
% function [cfg]=auto_fetch_system_information() 
%  
% % DO NOT EDITED THIS FILE
% %
% % This function was automatically generated by 
% % build_system.pl       
% %
% % It takes no arguemnts and returns the data structure 'cfg' 
% %
% % cfg.parameters contains a data structure of parameter informaiton
% %                .names
% %                .values
% %                .lower_bound
% %                .upper_bound
% %                .units
% %                      
% %                      
% % cfg.options.mi
% %                 
% % Contains fields corresponding to 
% % state, parameter, and output names that are used to map named values to their indices
% % in matlab. For example, if your system had a state Dp and it was the third state, 
% % you could get this information in the following way:
%  
% mymap = auto_map; 
% mymap.states.Dp
%
% % similarly the indices for parameters kel and outputs Dtot are accessed in the following
% % way:
%  
%  mymap.parameters.kel
%  mymap.outputs.Dtot

\n";

    print FHFETCH_SYSINFO $tmp_file_chunk ;
    print FHFETCH_SYSINFO "\n\n\n%creating cell array of parameter information\n";
    print FHFETCH_SYSINFO "p.matrix = [{'name'} " .&fetch_padding("name"          , $cfg->{parameters_length});
    print FHFETCH_SYSINFO " {'value'} "           .&fetch_padding(" {'value'}"     , $cfg->{parameter_values_length});
    print FHFETCH_SYSINFO " {'lb'} "              .&fetch_padding(" {'lb'}"        , $cfg->{parameter_values_length});
    print FHFETCH_SYSINFO " {'ub'} "              .&fetch_padding(" {'ub'}"        , $cfg->{parameter_values_length});
    print FHFETCH_SYSINFO " {'units'} "           .&fetch_padding(" {'units'}"     , $cfg->{parameter_text_length});
    print FHFETCH_SYSINFO " {'editable'} "        .&fetch_padding(" {'editable'}"  , $cfg->{parameter_text_length});
    print FHFETCH_SYSINFO " {'type'} "            .&fetch_padding(" {'type'}"      , $cfg->{parameter_text_length});
    print FHFETCH_SYSINFO " {'ptype'} "           .&fetch_padding(" {'ptype'}"     , $cfg->{parameter_text_length});

    #$counter = 1;
    foreach $parameter (@{$cfg->{parameters_index}}){
      print FHFETCH_SYSINFO "\n";     
      print FHFETCH_SYSINFO "            {'$parameter'} ".&fetch_padding($parameter, $cfg->{parameters_length});
      print FHFETCH_SYSINFO " $cfg->{parameters}->{$parameter}->{value} "          .&fetch_padding($cfg->{parameters}->{$parameter}->{value},       $cfg->{parameter_values_length});
      print FHFETCH_SYSINFO " $cfg->{parameters}->{$parameter}->{lower_bound} "    .&fetch_padding($cfg->{parameters}->{$parameter}->{lower_bound}, $cfg->{parameter_values_length});
      print FHFETCH_SYSINFO " $cfg->{parameters}->{$parameter}->{upper_bound} "    .&fetch_padding($cfg->{parameters}->{$parameter}->{upper_bound}, $cfg->{parameter_values_length});
      print FHFETCH_SYSINFO " {'$cfg->{parameters}->{$parameter}->{units}'} "      .&fetch_padding($cfg->{parameters}->{$parameter}->{units},       $cfg->{parameter_text_length});
      print FHFETCH_SYSINFO " {'$cfg->{parameters}->{$parameter}->{editable}'} "   .&fetch_padding($cfg->{parameters}->{$parameter}->{editable},    $cfg->{parameter_text_length});
      print FHFETCH_SYSINFO " {'$cfg->{parameters}->{$parameter}->{type}'} "       .&fetch_padding($cfg->{parameters}->{$parameter}->{type},        $cfg->{parameter_text_length});
      print FHFETCH_SYSINFO " {'$cfg->{parameters}->{$parameter}->{ptype}'} "      .&fetch_padding($cfg->{parameters}->{$parameter}->{ptype},     $cfg->{parameter_text_length});
      #$counter = $counter+1;
    }
    print FHFETCH_SYSINFO "];\n";     

    print FHFETCH_SYSINFO "\n\n\n% pulling out the info in fields \n";     

    print FHFETCH_SYSINFO "p.names       = p.matrix(2:end,1);\n";     
    print FHFETCH_SYSINFO "p.values      = cell2mat(p.matrix(2:end,2));\n";     
    print FHFETCH_SYSINFO "p.lower_bound = cell2mat(p.matrix(2:end,3));\n";     
    print FHFETCH_SYSINFO "p.upper_bound = cell2mat(p.matrix(2:end,4));\n";     
    print FHFETCH_SYSINFO "p.units       = p.matrix(2:end,5);\n";     
    print FHFETCH_SYSINFO "p.editable    = p.matrix(2:end,6);\n";     
    print FHFETCH_SYSINFO "p.type        = p.matrix(2:end,7);\n";     
    print FHFETCH_SYSINFO "p.ptype       = p.matrix(2:end,8);\n";     



    # 
    # creating matlab state, output and parameter map
    #
    print FHFETCH_SYSINFO  "\n\n\n %mapping state indices\n";
    $counter = 1;
    foreach $species (@{$cfg->{species_index}}){
      print FHFETCH_SYSINFO  " m.states.$species".&fetch_padding($species, $cfg->{species_length})."= $counter; \n";     
      $counter = $counter+1;
    }

    print FHFETCH_SYSINFO  "\n\n\n %mapping parameter indices\n";
    $counter = 1;
    foreach $parameter (@{$cfg->{parameters_index}}){
      print FHFETCH_SYSINFO  " m.parameters.$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= $counter; \n";     
      $counter = $counter+1;
    }

   # if there are IIV terms we dump the indices
   if(keys(%{$cfg->{iiv}})){
    foreach $set_id (keys %{$cfg->{iiv_index}}){
      $counter = 1;
      foreach $name  (@{$cfg->{iiv_index}->{$set_id}}){
        #print FHFETCH_SYSINFO  " m.iivs.$name".&fetch_padding($name, $cfg->{parameters_length})."= $counter; \n";     
        print FHFETCH_SYSINFO  " m.iiv.sets.$set_id.$name".&fetch_padding($name, $cfg->{parameters_length})."= $counter; \n";     
        $counter = $counter+1;
      }
    }
    print FHFETCH_SYSINFO  " m.iivs = m.iiv.sets.default; \n";
   }


    print FHFETCH_SYSINFO  "\n\n\n %mapping output indices\n";
    $counter = 1;
    foreach $output   (@{$cfg->{outputs_index}}){
      print FHFETCH_SYSINFO  " m.outputs.$output".&fetch_padding($output, $cfg->{outputs_length})."= $counter; \n";     
      $counter = $counter+1;
    }

    print FHFETCH_SYSINFO  "\n\n\n %mapping paramter sets indices\n";
    $counter = 1;
    print FHFETCH_SYSINFO  " m.parameter_sets_reverse = [];\n";
    foreach $set_id   (@{$cfg->{parameter_sets_index}}){
      print FHFETCH_SYSINFO  " m.parameter_sets_reverse = [m.parameter_sets_reverse {'$set_id'}];\n";
      print FHFETCH_SYSINFO  " m.parameter_sets.$set_id".&fetch_padding("m.parameter_sets.$set_id", $cfg->{parameters_length})."= $counter; \n";     
      $counter = $counter+1;
    }


    #
    # Dumping input information
    #
     
    # bolus inputs
    if(keys %{$cfg->{bolus_inputs}}){
    print FHFETCH_SYSINFO "\n\n\n% Adding bolus inputs\n";

    print FHFETCH_SYSINFO "inputs.bolus.times.values = $cfg->{bolus_inputs}->{times}->{values};\n";
    print FHFETCH_SYSINFO "inputs.bolus.times.scale  = '$cfg->{bolus_inputs}->{times}->{scale}';\n";
    print FHFETCH_SYSINFO "inputs.bolus.times.units  = '$cfg->{bolus_inputs}->{times}->{units}';\n";

      # now dumping bolus information for each state specified
      foreach $species (keys %{$cfg->{bolus_inputs}->{entries}}){
        print FHFETCH_SYSINFO   "inputs.bolus.species.$species.values =  $cfg->{bolus_inputs}->{entries}->{$species}->{values};\n";
        print FHFETCH_SYSINFO "inputs.bolus.species.$species.scale = '$cfg->{bolus_inputs}->{entries}->{$species}->{scale}';\n";
        print FHFETCH_SYSINFO "inputs.bolus.species.$species.units = '$cfg->{bolus_inputs}->{entries}->{$species}->{units}';\n";
      }
    }
    else{ print FHFETCH_SYSINFO  "\n\n% No bolus events were specified\n"; }

    # defining infusion rate inputs
    if(keys %{$cfg->{input_rates}}){
      print FHFETCH_SYSINFO  "\n\n% Defining infusion rates\n"; 
      print FHFETCH_SYSINFO   "inputs.infusion_rate_names = [{'".join("'}, {'", @{$cfg->{input_rates_index}})."'}];\n";
      foreach $name (@{$cfg->{input_rates_index}}){
        print FHFETCH_SYSINFO   "inputs.infusion_rates.$name.times.values  =  $cfg->{input_rates}->{$name}->{times}->{values};\n";
        print FHFETCH_SYSINFO   "inputs.infusion_rates.$name.times.scale   = '$cfg->{input_rates}->{$name}->{times}->{scale}';\n";
        print FHFETCH_SYSINFO   "inputs.infusion_rates.$name.times.units   = '$cfg->{input_rates}->{$name}->{times}->{units}';\n";

        print FHFETCH_SYSINFO   "inputs.infusion_rates.$name.levels.values = $cfg->{input_rates}->{$name}->{levels}->{values};\n";
        print FHFETCH_SYSINFO   "inputs.infusion_rates.$name.levels.scale  = '$cfg->{input_rates}->{$name}->{levels}->{scale}';\n";
        print FHFETCH_SYSINFO   "inputs.infusion_rates.$name.levels.units  = '$cfg->{input_rates}->{$name}->{levels}->{units}';\n";

      }
    }
    else{ print FHFETCH_SYSINFO  "\n\n% No infusion rates were specified\n"; }

    # defining time varying inputs
    if((@{$cfg->{covariates_index}})){
      print FHFETCH_SYSINFO  "\n\n% Defining the covariates \n"; 
      print FHFETCH_SYSINFO   "inputs.covariate_names = [{'".join("'}, {'", @{$cfg->{covariates_index}})."'}];\n";
      foreach $name (@{$cfg->{covariates_index}}){
        print FHFETCH_SYSINFO   "inputs.covariates.$name.cv_type       = '$cfg->{covariates}->{$name}->{cv_type}';\n";
        print FHFETCH_SYSINFO   "inputs.covariates.$name.times.units   = '$cfg->{covariates}->{$name}->{times}->{units}';\n";
        print FHFETCH_SYSINFO   "inputs.covariates.$name.values.units  = '$cfg->{covariates}->{$name}->{values}->{units}';\n";
        foreach $set_id   (keys %{$cfg->{covariates}->{$name}->{parameter_sets}}){
          print FHFETCH_SYSINFO   "inputs.covariates.$name.parameter_sets.$set_id.times  = $cfg->{covariates}->{$name}->{parameter_sets}->{$set_id}->{times};\n";
          print FHFETCH_SYSINFO   "inputs.covariates.$name.parameter_sets.$set_id.values = $cfg->{covariates}->{$name}->{parameter_sets}->{$set_id}->{values};\n";
        }
      }
    }
    else{ print FHFETCH_SYSINFO  "\n\n% No covariates were specified\n"; }

    # dumping the miscellaneous options
    if(keys %{$cfg->{options}}){
      foreach $name (keys %{$cfg->{options}}){
         print FHFETCH_SYSINFO   "misc.$name = '".&apply_format($cfg, $cfg->{options}->{$name}, 'matlab')."' ;\n";
      }
    }

    foreach $parameter (@{$cfg->{static_secondary_parameters_index}}){
      print FHFETCH_SYSINFO "misc.static_secondary_parameters.$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= '".&apply_format($cfg, $cfg->{static_secondary_parameters}->{$parameter}, 'matlab')."';\n";     
      if(defined($cfg->{if_conditional}->{$parameter})){
        print FHFETCH_SYSINFO "misc.static_secondary_parameters_conditional.$parameter = '".&extract_conditional($cfg, $parameter, 'matlab')."';\n";
      }
    }

    foreach $parameter (@{$cfg->{dynamic_secondary_parameters_index}}){
      print FHFETCH_SYSINFO "misc.dynamic_secondary_parameters.$parameter".&fetch_padding($parameter, $cfg->{parameters_length})."= '".&apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$parameter}, 'matlab')."';\n";     
      if(defined($cfg->{if_conditional}->{$parameter})){
        print FHFETCH_SYSINFO "misc.dynamic_secondary_parameters_conditional.$parameter = '".&extract_conditional($cfg, $parameter, 'matlab')."';\n";
      }
    }


    if(keys %{$cfg->{guides}}){
      foreach $name (keys %{$cfg->{guides}}){
         print FHFETCH_SYSINFO   "misc.guides.$name.type    = '$cfg->{guides}->{$name}->{type}' ;\n";
         print FHFETCH_SYSINFO   "misc.guides.$name.marker  = $cfg->{guides}->{$name}->{marker} ;\n";
         print FHFETCH_SYSINFO   "misc.guides.$name.color   = $cfg->{guides}->{$name}->{color} ;\n";
         print FHFETCH_SYSINFO   "misc.guides.$name.level   = $cfg->{guides}->{$name}->{level} ;\n";
      }

    }

    if(keys %{$cfg->{initial_conditions}}){
      print FHFETCH_SYSINFO "\n\n\n%Dumping non zero initial condition information\n";
      foreach $species (keys(%{$cfg->{initial_conditions}})){
        print FHFETCH_SYSINFO "initial_conditions.$species".&fetch_padding($species, $cfg->{species_length})."= '".&apply_format($cfg, $cfg->{initial_conditions}->{$species}, 'matlab')."'; \n";     
      }
    }


    if(($cfg->{time_scales_index})){
      print FHFETCH_SYSINFO "\n\n\n%Dumping timescale information \n";
      foreach $name  (@{$cfg->{time_scales_index}}){
        print FHFETCH_SYSINFO         
              "time_scales.$name ".&fetch_padding($name  , $cfg->{time_scales_length})."= $cfg->{time_scales}->{$name}; \n";     
      }
    }

    if((@{$cfg->{data}->{headers}->{values}})){
      print FHFETCH_SYSINFO "\n\n\n%loading the data file \n";
      print FHFETCH_SYSINFO "data.data_file.name ".&fetch_padding("name"  , $cfg->{parameters_length})." = ";
      print FHFETCH_SYSINFO "'".$cfg->{data}->{file}."';\n";
      foreach $name  (@{$cfg->{data}->{headers}->{values}}){
        print FHFETCH_SYSINFO "data.column.names.$name".&fetch_padding($name  , $cfg->{parameters_length})." = '$name';\n";
      }
      print FHFETCH_SYSINFO "data = nm_read_data(data); \n";
    }
    else{
      print FHFETCH_SYSINFO "data = struct(); \n"; }

    #
    # dumping the parameter sets
    #
    # Defining the default set
    print FHFETCH_SYSINFO "\n\n\n%Defining parameter sets\n";     
    foreach $set_id   (@{$cfg->{parameter_sets_index}}){
      # defaulting the values in set_name to the default values
      print FHFETCH_SYSINFO "p.sets.$set_id.values  = p.values;\n"; 

      # if a name as been specified then we set that here otherwise we 
      # just default the name field to set_id
      if(defined($cfg->{parameter_sets}->{$set_id}->{name})){
        print FHFETCH_SYSINFO "p.sets.$set_id.name ".&fetch_padding($set_id, $cfg->{parameters_length})."= '$cfg->{parameter_sets}->{$set_id}->{name}';\n";     }
      else{
        print FHFETCH_SYSINFO "p.sets.$set_id.name ".&fetch_padding($set_id, $cfg->{parameters_length})."= '$set_id';\n";     }

      # now we overwrite each parameter
      foreach $name  (keys(%{$cfg->{parameter_sets}->{$set_id}->{values}})){
        print FHFETCH_SYSINFO "p.sets.$set_id.values(m.parameters.$name) ".&fetch_padding($name, $cfg->{parameters_length})." = $cfg->{parameter_sets}->{$set_id}->{values}->{$name};\n";     }
           
    } 

    
    print FHFETCH_SYSINFO "\n\n\n%Defining IIV information\n";     

   if(keys(%{$cfg->{iiv}})){
     # If we have IIV information we dump it based on the 
     # parameter sets. There should be at least a 'default' 
     # set of parameters (set_id)
     foreach $set_id (keys %{$cfg->{iiv_index}}){
       if(($cfg->{iiv_index}->{$set_id})){
         foreach $name     (@{$cfg->{iiv_index}->{$set_id}}){
           print FHFETCH_SYSINFO "iiv.sets.$set_id.iivs.$name.parameters ".&fetch_padding($name, $cfg->{parameters_length})." =['{".join("}', '{", @{$cfg->{iiv}->{$set_id}->{iivs}->{$name}->{parameters}})."}'];\n";
         }
         foreach $name     (keys(%{$cfg->{iiv}->{$set_id}->{parameters}})){
           print FHFETCH_SYSINFO "iiv.sets.$set_id.parameters.$name.iiv_name    ".&fetch_padding($name, $cfg->{parameters_length})." = '".$cfg->{iiv}->{$set_id}->{parameters}->{$name}->{iiv_name}."';\n";
           print FHFETCH_SYSINFO "iiv.sets.$set_id.parameters.$name.distribution".&fetch_padding($name, $cfg->{parameters_length})." = '".$cfg->{iiv}->{$set_id}->{parameters}->{$name}->{distribution}."';\n";
           print FHFETCH_SYSINFO "iiv.sets.$set_id.parameters.$name.equation    ".&fetch_padding($name, $cfg->{parameters_length})." = '".&make_iiv($cfg, $name, 'matlab', $set_id)."';\n";
         }
       
         print FHFETCH_SYSINFO "iiv.sets.$set_id.values = zeros(".scalar(@{$cfg->{iiv_index}->{$set_id}}).",".scalar(@{$cfg->{iiv_index}->{$set_id}}).");\n";
         $counter = 1;
         foreach $name     (@{$cfg->{iiv_index}->{$set_id}}){
           $counter2 = 1;
           foreach $name2    (@{$cfg->{iiv_index}->{$set_id}}){
             if(defined($cfg->{iiv}->{$set_id}->{vcv}->{$name}->{$name2})){
               print FHFETCH_SYSINFO "iiv.sets.$set_id.values($counter, $counter2) =  $cfg->{iiv}->{$set_id}->{vcv}->{$name}->{$name2};\n";
             }
             $counter2 = $counter2+1;
           }
         $counter = $counter+1;
         }
       
         print FHFETCH_SYSINFO "% Checking to make sure the variance/covariance  \n";
         print FHFETCH_SYSINFO "% information makes sense \n";
         print FHFETCH_SYSINFO "if(min(eig((iiv.sets.$set_id.values + iiv.sets.$set_id.values')./2)) <=0)\n";
         print FHFETCH_SYSINFO "  disp('Warning: IIV variance/covariance matrix is not')\n";
         print FHFETCH_SYSINFO "  disp('positive semi-definiate, so it will not be possible')\n";
         print FHFETCH_SYSINFO "  disp('to perform stochastic simulations with interaction')\n";
         print FHFETCH_SYSINFO "  disp('for Parmaeter set $set_id')\n";
         print FHFETCH_SYSINFO "end\n";
       
        }
      }
      print FHFETCH_SYSINFO "iiv.current_set = 'default'                  ;\n";
      print FHFETCH_SYSINFO "iiv.iivs        = iiv.sets.default.iivs      ;\n";
      print FHFETCH_SYSINFO "iiv.parameters  = iiv.sets.default.parameters;\n";
      print FHFETCH_SYSINFO "iiv.values      = iiv.sets.default.values    ;\n";
    }
    else{
      print FHFETCH_SYSINFO "iiv      = struct(); \n"; }

    print FHFETCH_SYSINFO "\n\n\n%Defining variance equations\n";     



  if ((keys(%{$cfg->{variance}->{equations}}))){
     foreach $name  (keys(%{$cfg->{variance}->{equations}})){
        print FHFETCH_SYSINFO "ve.$name = '".&apply_format($cfg, $cfg->{variance}->{equations}->{$name},'matlab')."';\n";
     }
    }
  else{
    print FHFETCH_SYSINFO "% No variance equations defined\n";
    print FHFETCH_SYSINFO "% See: <VE>\n";
    print FHFETCH_SYSINFO "ve      = struct(); \n"; }


$tmp_file_chunk    = "\n\n\n% Creating the cfg data structures
% basically pulling it all together
cfg.parameters      = p;
cfg.options.mi      = m; 
cfg.iiv             = iiv;
cfg.ve              = ve; 
cfg.data.default    = data; 
cfg.options.verbose = 'yes'; %defaulting to displaying information
if(exist('inputs', 'var'))
  cfg.options.inputs = inputs;
end

% default simulation options:
cfg.options.simulation_options.model_name             = 'ode_simulation';
cfg.options.simulation_options.default_simopts.Solver = 'ode23s';
cfg.options.simulation_options.output_times           = linspace(0,10,1000)';
cfg.options.simulation_options.integrate_with                  = 'm-file';
cfg.options.simulation_options.include_important_output_times  = 'yes';

% hidden option to control printing in simulations
cfg.options.simulation_options.logging  = 'yes';

cfg.options.logging.enabled   = 'yes';
cfg.options.logging.file      = sprintf('transient%subiquity_log.txt', filesep);
cfg.options.logging.timestamp = 'yes';
cfg.options.logging.ts_str    = 'yyyy-mm-dd HH:MM:SS';


%Defining the default stochastic options
cfg.options.stochastic.nsub    = 100;
cfg.options.stochastic.seed    = 8675309;
cfg.options.stochastic.ci      = 95;
cfg.options.stochastic.ponly   = false ;

% By default all states and outputs will be included
cfg.options.stochastic.states  = fieldnames(cfg.options.mi.states);
cfg.options.stochastic.outputs = fieldnames(cfg.options.mi.outputs);

% Defining an empty cohort data structure:
cfg.cohorts                  = struct();

% Defining the default estimation options
cfg.estimation.observation_function           = 'system_od_general';
cfg.estimation.effort                         = 1;
cfg.estimation.optimizer                      = 'fminsearch';
cfg.estimation.monitor.status_function        = 'estimation_status';
cfg.estimation.monitor.exit_when_stable       = 'no';
cfg.estimation.monitor.iteration_history      = 100;
cfg.estimation.monitor.slope_tolerance        = 0.001;
cfg = system_select_set(cfg, 'default');

if(exist('misc', 'var'))
  cfg.options.misc = misc;
else
  cfg.options.misc = {};
end

if(exist('initial_conditions', 'var'))
  cfg.options.initial_conditions = initial_conditions;
end

if(exist('time_scales', 'var'))
 cfg.options.time_scales = time_scales;
end

\n";

    print FHFETCH_SYSINFO $tmp_file_chunk ;

  # appending the comments
  $tmp_file_chunk = $cfg->{comments};
  $tmp_file_chunk = &fetch_comments($tmp_file_chunk, 'matlab');
  $tmp_file_chunk =  "cfg.options.model_details = ".$tmp_file_chunk.";";


    print FHFETCH_SYSINFO $tmp_file_chunk ;


    #
    # creating the simulation driver
    #
    # mapping function for simulation output
   $tmp_file_chunk    = "function [som]=auto_map_simulation_output(so,cfg) 
% function [som]=auto_map_simulation_output(so,cfg) 
% % DO NOT EDITED THIS FILE
% %
% % This function was automatically generated by 
% % build_system.pl       
% %
% % It takes one argume (so), the output from run_simulation_generic and returns the data
% % structure 'som' with named fields for times, states, and outputs.

m = cfg.options.mi;

% the raw simulation output 
som.raw      = so;
som.times.sim_time = so.t; \n ";


    print FHMAP_SIMULATION_OUTPUT $tmp_file_chunk;

    print FHMAP_SIMULATION_OUTPUT "\n\n\n% Mapping state output\n";
    foreach $species (@{$cfg->{species_index}}){
      print FHMAP_SIMULATION_OUTPUT 
            "som.states.$species ".&fetch_padding($species, $cfg->{species_length})."= so.x(:,m.states.$species); \n";     
    }

    print FHMAP_SIMULATION_OUTPUT "\n\n\n% Mapping output output\n";
   
    foreach $output (@{$cfg->{outputs_index}}){
      print FHMAP_SIMULATION_OUTPUT 
            "som.outputs.$output  ".&fetch_padding($output , $cfg->{outputs_length})."= so.y(:,m.outputs.$output); \n";     
    }

    print FHMAP_SIMULATION_OUTPUT "\n\n\n% Mapping time scales\n";
   
    foreach $name  (@{$cfg->{time_scales_index}}){
      print FHMAP_SIMULATION_OUTPUT 
            "som.times.$name ".&fetch_padding($name  , $cfg->{time_scales_length})."= so.t*$cfg->{time_scales}->{$name}; \n";     
    }


    #
    # M-file sim 
    #
    # Flow of the file
    #   - call odexx to simulate the system
    #
    #  loop through each time
    #     - define common_block
    #     - calculate outputs (y)
    #
    #
$tmp_file_chunk    = "function [SIMINT_t, SIMINT_x, y] = auto_sim(S, SIMINT_output_times, SIMINT_initialstate)
% % DO NOT EDITED THIS FILE
% %
% % This function was automatically generated by 
% % build_system.pl       
% %
% % It is called internally by run_simulation_generic.m

";
    print FHSIMM $tmp_file_chunk;

    print FHSIMM "% Running simulation\n";

    print FHSIMM "SIMINT_runsim_string  = sprintf('[SIMINT_t, SIMINT_x] = %s(\@auto_odes, SIMINT_output_times, SIMINT_initialstate,[],S);', S.default_simopts.Solver);\n";
    print FHSIMM "\neval(SIMINT_runsim_string);\n";

    print FHSIMM "\n%\n% Mapping outputs\n%\n";
    print FHSIMM "for SIMINT_TIMEIDX=1:length(SIMINT_t)\n";
    print FHSIMM "SIMINT_TIME = SIMINT_t(SIMINT_TIMEIDX);\n";
    print FHSIMM "% Pulling out the states for the current time\n";
    print FHSIMM "x = interp1(SIMINT_t, SIMINT_x, SIMINT_TIME);\n";

    print FHSIMM $m_common_block;

    print FHSIMM $m_outputs ;
    print FHSIMM "end\n";

    #
    # M-file odes
    #
    #  Flow of the file:
    #   - map parameters     (interpolate at t)
    #   - map rate inputs    (interpolate at t)
    #   - map covariates     (interpolate at t)
    #   - map covariates_ic  (interpolate at first time)
    #   - create static algebraic values
    #   - map states to state names
    #   - create dynamic states/conditionals
    #   - define odes
    #   - map odes back to dx

$tmp_file_chunk    = "function [dx] = auto_odes(SIMINT_TIME,x,S)
% % DO NOT EDITED THIS FILE
% %
% % This function was automatically generated by 
% % build_system.pl       
% %
% % It is called internally by run_simulation_generic.m
";
    print FHODESM $tmp_file_chunk;
    print FHODESM $m_common_block;
    print FHODESM $m_odes;


    #
    # Simulation Driver
    #


my $simulation_driver_ph = {
     BOLUS                  =>'',
     OUTPUT_TIMES           =>'',
     PSETS                  =>'',
     INFUSION_RATES         =>'',
     PLOT_OUTPUT            =>'',
     PLOT_OUTPUT_STOCHASTIC =>'',
     COVARIATES             =>''};


my $simulation_driver_template = "clear; close all;
% compiling the system to make sure the 
% latest changes have been committed. 
build_system

% setting up paths
provision_workspace;

% pulling out the system information
cfg  = auto_fetch_system_information();

<PSETS>
% selecting the default parameter set:
cfg  = system_select_set(cfg, 'default');


% fetching the parameter values
parameters = system_fetch_parameters(cfg);

% The previous statement sets 'parameters' to the values 
% in the currently selected parameter set. To overwrite 
% a specific parameter uncomment the following statement 
% and replace PNAME with the name of the parameter 
% and VALUE with the desired value:
%
% parameters = system_set_parameter(cfg, parameters, 'PNAME', VALUE);

<BOLUS>

<INFUSION_RATES>

<COVARIATES>

<OUTPUT_TIMES>

% Options to control simulation execution and outputs are defined here.
% To see a list of options type:
% help system_set_option
%
% Here are a few useful options:
% To force matlab to use simulink uncomment the following line:
% cfg = system_set_option(cfg, 'simulation', 'integrate_with', 'simulink');
%
% To change the ODE solver that is used:
% cfg = system_set_option(cfg, 'solver', 'solver', 'ode23s');


% -----------------------------------------------------------------------
% Indiviudal Simulation:
% Simulating the system and storing the result in  som (Simulation Output
% Mapped) -- a data structure with times, states and outputs mapped to 
% their internal names
som = run_simulation_ubiquity(parameters, cfg);


% the following just plots all of the 
% specified outputs
figure(1);
hold on;

<PLOT_OUTPUT>

prepare_figure('present');
% set(gca, 'yscale', 'log');
  xlabel('time');
  ylabel('outputs');
% -----------------------------------------------------------------------



% -----------------------------------------------------------------------
% Stochastic Simulation
% predictions = simulate_subjects(parameters, cfg)
% mc = fetch_color_codes;

<PLOT_OUTPUT_STOCHASTIC>
% -----------------------------------------------------------------------

";

    # 
    # BOLUS INPUTS 
    # 
    if(keys %{$cfg->{bolus_inputs}}){
      $simulation_driver_ph->{BOLUS} .= "% %cfg=system_zero_inputs(cfg);\n";
      foreach $species (keys %{$cfg->{bolus_inputs}->{entries}}){
        $simulation_driver_ph->{BOLUS} .= "% cfg=system_set_bolus(cfg, '$species', ...  % \n";
        $simulation_driver_ph->{BOLUS} .= "%                            $cfg->{bolus_inputs}->{times}->{values}, ...  % $cfg->{bolus_inputs}->{times}->{units}\n";
        $simulation_driver_ph->{BOLUS} .= "%                            $cfg->{bolus_inputs}->{entries}->{$species}->{values});     % $cfg->{bolus_inputs}->{entries}->{$species}->{units} \n \n"; 
        $ae->{BOLUS}.= "cohort.inputs.bolus.$species.TIME".&fetch_padding($species, 19)."= [];\n";
        $ae->{BOLUS}.= "cohort.inputs.bolus.$species.AMT ".&fetch_padding($species, 19)."= [];\n";

        $counter = $counter + 1;
      }
    }
    else{ $simulation_driver_ph->{BOLUS} .= "\n% No bolus inputs defined\n"; }

    # 
    # INFUSION RATES
    # 
    if(keys %{$cfg->{input_rates}}){
      $simulation_driver_ph->{INFUSION_RATES} .= "% Defining infusion rates\n"; 
      foreach $name (@{$cfg->{input_rates_index}}){
        $simulation_driver_ph->{INFUSION_RATES} .="%  cfg = system_set_rate(cfg, '$name', ...\n";
        $simulation_driver_ph->{INFUSION_RATES} .="%                              $cfg->{input_rates}->{$name}->{times}->{values}, ... % $cfg->{input_rates}->{$name}->{times}->{units}  \n";
        $simulation_driver_ph->{INFUSION_RATES} .="%                              $cfg->{input_rates}->{$name}->{levels}->{values});   % $cfg->{input_rates}->{$name}->{levels}->{units} \n";
        $ae->{INFUSION_RATES}.= "cohort.inputs.infusion_rates.$name.TIME".&fetch_padding($name,   10)."= [];\n";
        $ae->{INFUSION_RATES}.= "cohort.inputs.infusion_rates.$name.AMT ".&fetch_padding($name,   10)."= [];\n";
      }
    }
    else{ $simulation_driver_ph->{INFUSION_RATES} .= "% No infusion rates defined\n"; }
    # 
    # Covaraites
    # 
    if(keys %{$cfg->{covariates}}){
      $simulation_driver_ph->{COVARIATES} .= "% Covariates can change with parameter sets. What's shown here \n";
      $simulation_driver_ph->{COVARIATES} .= "% are the default values. Uncomment these to overwrite the values \n";
      $simulation_driver_ph->{COVARIATES} .= "% specified by the system_select_set command above. \n";
      foreach $name (@{$cfg->{covariates_index}}){
        $simulation_driver_ph->{COVARIATES} .= "% Covariate $name \n";
        $simulation_driver_ph->{COVARIATES} .= "% Type:       $cfg->{covariates}->{$name}->{cv_type}  \n";
        $simulation_driver_ph->{COVARIATES} .= "% Time units: $cfg->{covariates}->{$name}->{times}->{units}\n";
        $simulation_driver_ph->{COVARIATES} .= "% Cov units:  $cfg->{covariates}->{$name}->{values}->{units}\n";
        $simulation_driver_ph->{COVARIATES} .= "% cfg = system_set_covariate(cfg, '$name', ... \n";
        $simulation_driver_ph->{COVARIATES} .= "%                                  $cfg->{covariates}->{$name}->{parameter_sets}->{default}->{times}, ...\n";
        $simulation_driver_ph->{COVARIATES} .= "%                                  $cfg->{covariates}->{$name}->{parameter_sets}->{default}->{values});\n\n";
        $ae->{COVARIATES}.= "cohort.inputs.covariates.$name.TIME".&fetch_padding($name,   14)."= [];\n";
        $ae->{COVARIATES}.= "cohort.inputs.covariates.$name.AMT ".&fetch_padding($name,   14)."= [];\n";
      }
    }
    else{ $simulation_driver_ph->{COVARIATES} .= "% No covariates found\n"; }


    # 
    # Parameter sets
    # 
    if(keys %{$cfg->{parameter_sets}}){
      $simulation_driver_ph->{PSETS} .= "% set name".fetch_padding("set name", 25)." | Description\n";
      $simulation_driver_ph->{PSETS} .= "% -------------------------------------------------------\n";
      foreach $name (@{$cfg->{parameter_sets_index}}){
        $simulation_driver_ph->{PSETS} .= "% $name".fetch_padding($name, 25)." | $cfg->{parameter_sets}->{$name}->{name}\n";
      }

      # this block is the same in simulation driver and the analysis
      # estimation template 
      $ae->{PSETS} = $simulation_driver_ph->{PSETS}
    }

    # 
    # Defining the default output times
    # 
    $simulation_driver_ph->{OUTPUT_TIMES} .= "\n% Evaluate system at the following output times\n";
    if(defined($cfg->{options}->{output_times})){
       $ae->{OUTPUT_TIMES} = &apply_format($cfg, $cfg->{options}->{output_times}, 'matlab')."'";
       $simulation_driver_ph->{OUTPUT_TIMES} .= "cfg = system_set_option(cfg, 'simulation', 'output_times', ".&apply_format($cfg, $cfg->{options}->{output_times}, 'matlab')."');\n";}
    else{
       $ae->{OUTPUT_TIMES} = 'linspace(0,100,1000)\'';
       $simulation_driver_ph->{OUTPUT_TIMES} .= "cfg = system_set_option(cfg, 'simulation', 'output_times', linspace(0,100,1000)');\n";}

    if(defined($cfg->{outputs_index})){
     $simulation_driver_ph->{PLOT_OUTPUT}.="% You can access different parts of the\n% simulation using the som variable.\n";
     $simulation_driver_ph->{PLOT_OUTPUT}.="% som.times.TIMESCALE \n";  
     $simulation_driver_ph->{PLOT_OUTPUT}.="% som.outputs.OUTPUTNAME \n";  
     $simulation_driver_ph->{PLOT_OUTPUT}.="% som.states.STATENAME \n"; 
      
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="% % Uncomment the lines below and substiute the          \n";
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="% % desired timescale and output for TS and OUTPUT       \n";
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="% patch(predictions.times_patch.TS, ...                  \n";
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="%       predictions.outputs_patch.OUTPUT.ci, ...         \n";
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="%       mc.light_blue, 'edgecolor', 'none');           \n\n";
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="% % This plots the mean:                                 \n"; 
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="%   plot(predictions.times.weeks, ...                    \n";                      
     $simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}.="%        predictions.outputs_stats.Cp_Total.mean, 'b-'); \n";    
     if(defined($cfg->{options}->{TS})){
       $simulation_driver_ph->{PLOT_OUTPUT}.="plot(som.times.".$cfg->{options}->{TS}.", ...\n"; }
     else{
       $simulation_driver_ph->{PLOT_OUTPUT}.="plot(som.times.sim_time, ...\n"; }
     $simulation_driver_ph->{PLOT_OUTPUT}.="     som.outputs.".$cfg->{outputs_index}->[0].", 'b-'); \n"; 
    }
    else{
     $simulation_driver_ph->{PLOT_OUTPUT}.=" % No outputs were defined\ndefine some using <O> OUTPUT=EXPRESSOIN\n"; }




    # subbing in the various components
    $simulation_driver_template =~ s#<BOLUS>#$simulation_driver_ph->{BOLUS}#;
    $simulation_driver_template =~ s#<INFUSION_RATES>#$simulation_driver_ph->{INFUSION_RATES}#;
    $simulation_driver_template =~ s#<OUTPUT_TIMES>#$simulation_driver_ph->{OUTPUT_TIMES}#;
    $simulation_driver_template =~ s#<PSETS>#$simulation_driver_ph->{PSETS}#;
    $simulation_driver_template =~ s#<PLOT_OUTPUT>#$simulation_driver_ph->{PLOT_OUTPUT}#;
    $simulation_driver_template =~ s#<PLOT_OUTPUT_STOCHASTIC>#$simulation_driver_ph->{PLOT_OUTPUT_STOCHASTIC}#;
    $simulation_driver_template =~ s#<COVARIATES>#$simulation_driver_ph->{COVARIATES}#;


    print FHSIM_DRIVER $simulation_driver_template;
    
    close(FHCOMMON_BLOCK);    
    close(FHINITIALIZE);
    close(FHODES);                
    close(FHREMAP_ODES);          
    close(FHFETCH_SYSINFO);
    close(FHSIM_DRIVER);
    close(FHOUTPUTS);
    close(FHMAP_SIMULATION_OUTPUT);
    close(FHODESM);                
    close(FHSIMM);                

    # JMH update the estimation script to reflect changes

    foreach $field     (keys(%{$ae})){
        $ae_template =~ s#<$field>#$ae->{$field}#g;
    }

    open(FHESTIMATION,            '>', &ftf($cfg, $cfg->{files}->{analysis_estimation}));
    print FHESTIMATION $ae_template;
    close(FHESTIMATION);

}


sub dump_potterswheel3
{
   my ($cfg, $parameter_set) = @_;

   # getting the template from below
   my $pwtemplate = &fetch_pw_template3;

   my $data = {
       odes                 =>'',
       outputs              =>'',
       parameters           =>'',
       inputs               =>'',
       secondary_parameters =>'',
       states               =>''
   };
 
   my $entry_template = {
      ode                  =>'m = pwAddODE(m,  <STATE>,     \'<ODE> \'   );',    #done 
      output               =>'m = pwAddY(m,   \'<OUTPUT>\',  \'<RHS>\');',
      parameter            =>'m = pwAddK(m,     <NAME>,        <VALUE>,  \'fix\', <MINVALUE>,   <MAXVALUE>);', 
      input                =>'m = pwAddU(m,     <NAME>,      \'<TYPE>\',  <TIMES>, <MAGNITUDES>, [], [], [], [], [], [], [], [], [], \'x+u * <SCALE>\');',
      secondary_parameter  =>'m = pwAddA(m,     <NAME>,      \'<VALUE>\');',  # done
      initial_condition    =>'m = pwAddTE(m,    <TE_STATE>,    <STATE>,    \'<VALUE>\');', 
      state                =>'m = pwAddX(m,     <STATE>,     0, \'fix\',    [], [],   [], \'Central\');' #done
   };

   #secondary_parameter  =>'m = pwAddA(m,     <NAME>,      \'<VALUE>\',   [], [], [], [], \'parameter\');',  # done

   my $name;
   my $tmprhs;
   my $tmpstr;
   my $paddedstr;
   my $tmpvalue;


   #$data->{outputs}= $data->{outputs}."\n\n% default scaling parameter of 1\nm = pwAddS(m, 'scale_none', 1, 'fix');\n";

   # processing outputs
   if(keys %{$cfg->{outputs}}){
     foreach $name (keys %{$cfg->{outputs}}){
      $tmpstr = $entry_template->{output};

      # formatting to language
      $tmpvalue =  &apply_format($cfg, $cfg->{outputs}->{$name}, 'pw');

      $tmpstr =~ s#<RHS>#$tmpvalue#; 
      $tmpstr =~ s#<OUTPUT>#$name#; 
      $data->{outputs}= $data->{outputs}.$tmpstr."\n";
     }
   }

   # parameters
   if(keys %{$cfg->{parameters}}){
     foreach $name  (keys(%{$cfg->{parameters}})){
       $tmpstr = $entry_template->{parameter};
       $paddedstr = "'".$name."'".&fetch_padding($name, $cfg->{parameters_length});
       $tmpstr =~ s#<NAME>#$paddedstr#;

      if(exists($cfg->{parameter_sets}->{$parameter_set}->{values}->{$name})){
         $paddedstr = $cfg->{parameter_sets}->{$parameter_set}->{values}->{$name}.&fetch_padding($cfg->{parameter_sets}->{$parameter_set}->{values}->{$name}, $cfg->{parameter_values_length}); }
      else{
         $paddedstr = $cfg->{parameters}->{$name}->{value}.&fetch_padding($cfg->{parameters}->{$name}->{value}, $cfg->{parameter_values_length}); }
       $tmpstr =~ s#<VALUE>#$paddedstr#;
       $tmpstr =~ s#<MAXVALUE>#$cfg->{parameters}->{$name}->{upper_bound}#;
       $tmpstr =~ s#<MINVALUE>#$cfg->{parameters}->{$name}->{lower_bound}#;
       $data->{parameters}= $data->{parameters}.$tmpstr."\n";
     }
      
   }

   # creating a link from the simulation time to SIMINT_TIME
   # JMH find the potters wheel name for time, insert it into ?value? and uncomment the
   # following lines:
   # $tmpstr                       = $entry_template->{secondary_parameter};
   # $tmpstr                                      =~ s#<NAME>#SIMINT_TIME#;
   # $tmpstr                                      =~ s#<VALUE>#?value?#;
   # $data->{secondary_parameters}                =  $data->{secondary_parameters}.$tmpstr."\n";

   # secondary parameters
   # static 
   foreach $name  (@{$cfg->{static_secondary_parameters_index}}){
     if(defined($cfg->{static_secondary_parameters}->{$name})){
       $tmpstr                       = $entry_template->{secondary_parameter};
       # formatting to language
       $tmpvalue =  &apply_format($cfg, $cfg->{static_secondary_parameters}->{$name}, 'pw');
       $paddedstr = "'".$name."'".&fetch_padding($name, $cfg->{parameters_length});
       $tmpstr                                      =~ s#<NAME>#$paddedstr#;
       $tmpstr                                      =~ s#<VALUE>#$tmpvalue#;
       $data->{secondary_parameters}                =  $data->{secondary_parameters}.$tmpstr."\n";
     }
   }


   # dynamic
   foreach $name  (@{$cfg->{dynamic_secondary_parameters_index}}){
     if(defined($cfg->{dynamic_secondary_parameters}->{$name})){
       $tmpstr                                      = $entry_template->{secondary_parameter};
       $paddedstr = "'".$name."'".&fetch_padding($name, $cfg->{parameters_length});
       $tmpstr                                      =~ s#<NAME>#$paddedstr#;
       # checking to see if a conditional statement has been made for this parameter
       if(defined($cfg->{if_conditional}->{$name})){
         $tmprhs =  &extract_conditional($cfg, $name, 'pw') ;
         $tmpstr =~ s#<VALUE>#$tmprhs#;
       }
       else{
         # formatting to language
         $tmpvalue  =  &apply_format($cfg, $cfg->{dynamic_secondary_parameters}->{$name}, 'pw');
         $tmpstr    =~ s#<VALUE>#$tmpvalue#;
       }
       $data->{secondary_parameters}                =  $data->{secondary_parameters}.$tmpstr."\n";

     }
   }

   # processing states
   foreach $name  (@{$cfg->{species_index}}){

     # state declaration
     $tmpstr    = $entry_template->{state};
     $paddedstr = "'".$name."'".&fetch_padding($name, $cfg->{species_length});
     $tmpstr    =~ s#<STATE>#$paddedstr#;
     $data->{states} =  $data->{states}.$tmpstr."\n";


     # creating odes
     if(defined($cfg->{species}->{$name})){
       $tmprhs = '';
       $tmpstr    = '';
       if((@{$cfg->{species}->{$name}->{production}})){
         # formatting to language
         $tmprhs = $tmprhs.&apply_format($cfg, join(' + ',@{$cfg->{species}->{$name}->{production}}), 'pw');}
           
       if((@{$cfg->{species}->{$name}->{consumption}})){
          # formatting to language
          $tmprhs = $tmprhs."-(".&apply_format($cfg, join(' + ',@{$cfg->{species}->{$name}->{consumption}}), 'pw').")";}
       if((@{$cfg->{species}->{$name}->{odes}})){
           $tmprhs = $tmprhs."+".&apply_format($cfg, join(' + ',@{$cfg->{species}->{$name}->{odes}}), 'pw'); }


       # adding the line to the 'odes' hash
       $tmpstr       = $entry_template->{ode};
       $paddedstr = "'".$name."'".&fetch_padding($name, $cfg->{species_length});
       $tmpstr       =~ s#<STATE>#$paddedstr#;
       $tmpstr       =~ s#<ODE>#$tmprhs#;
       $data->{odes} =  $data->{odes}.$tmpstr."\n";
     }

     # inital conditions
     if(defined($cfg->{initial_conditions}->{$name})){
       $tmpstr                             = $entry_template->{initial_condition};
       # language specific formatting
       $tmpvalue                           = &apply_format($cfg, $cfg->{initial_conditions}->{$name}, 'pw');
       $paddedstr = "'TE_".$name."'".&fetch_padding("TE_$name", $cfg->{parameters_length});
       $tmpstr                             =~ s#<TE_STATE>#$paddedstr#;
       $paddedstr = "'".$name."'".&fetch_padding($name, $cfg->{parameters_length});
       $tmpstr                             =~ s#<STATE>#$paddedstr#;
       $tmpstr                             =~ s#<VALUE>#$cfg->{initial_conditions}->{$name}#;
       $data->{secondary_parameters}       =  $data->{secondary_parameters}.$tmpstr."\n";
     }
   }

   #inputs
    
   #First defining bolus inputs
   if(defined($cfg->{bolus_inputs})){
     if(defined($cfg->{bolus_inputs}->{entries}) and
        defined($cfg->{bolus_inputs}->{times})){
      foreach $name (keys %{$cfg->{bolus_inputs}->{entries}}){
       $tmpstr                             = $entry_template->{input};
       $paddedstr = "'$name'".&fetch_padding("injection_$name", $cfg->{inputs_length});
       $tmpstr                             =~ s#<NAME>#$paddedstr#;
       $tmpstr                             =~ s#<TYPE>#injection#;
       $tmpstr                             =~ s#<TIMES>#$cfg->{bolus_inputs}->{times}->{values}.*$cfg->{bolus_inputs}->{times}->{scale}#;
       $tmpstr                             =~ s#<MAGNITUDES>#$cfg->{bolus_inputs}->{entries}->{$name}->{values}#;
       $tmpstr                             =~ s#<SCALE>#$cfg->{bolus_inputs}->{entries}->{$name}->{scale}#;
       #JMH add magnitude scale (not sure where to stick it right now)
       $data->{inputs}                     =  $data->{inputs}.$tmpstr."\n";
      }
    }
   
   }

   if(%{$cfg->{input_rates}}){
    foreach $name (keys %{$cfg->{input_rates}}){
      $tmpstr                             = $entry_template->{input};
      $tmpstr                             =~ s#<TYPE>#steps#;
      $tmpstr                             =~ s#<COMPARTMENT>##;
      $paddedstr = "'".$name."'".&fetch_padding($name, $cfg->{inputs_length});
      $tmpstr                             =~ s#<NAME>#$paddedstr#;
      $tmpstr                             =~ s#<TIMES>#$cfg->{input_rates}->{$name}->{times}->{values}.*$cfg->{input_rates}->{$name}->{times}->{scale}#;
      $tmpstr                             =~ s#<MAGNITUDES>#$cfg->{input_rates}->{$name}->{levels}->{values}#;
      $tmpstr                             =~ s#<SCALE>#$cfg->{input_rates}->{$name}->{levels}->{scale}#;
      $data->{inputs}                     =  $data->{inputs}.$tmpstr."\n";
    }
  }


    # substituting the components in the template for the actual values
    $pwtemplate =~ s#<OUTPUTS>#$data->{outputs}#;
    $pwtemplate =~ s#<ODES>#$data->{odes}#;
    $pwtemplate =~ s#<STATES>#$data->{states}#;
    $pwtemplate =~ s#<PARAMETERS>#$data->{parameters}#;
    $pwtemplate =~ s#<SECONDARY_PARAMETERS>#$data->{secondary_parameters}#;
    $pwtemplate =~ s#<INPUTS>#$data->{inputs}#;

 

   # dumping the potterswheel model file 
   open(FHPW,    '>', &ftf($cfg, $cfg->{files}->{potterswheel3}."-$parameter_set.m"));
   print FHPW $pwtemplate;
   close(FHPW);
}

sub fortranify_line
{

  # This subroutine takes a string and returns properly formatted fortran code. The string
  # should contain no carriage returns or new line characters. It should also contain no
  # preceding spaces. These will be added internally. If, after adding the spaces the
  # resulting string is longer than 72 characters, it will be wrapped at a space or
  # operator.
  #
  # The following three lines should test the different expected scenarios:
  #  print &fortranify_line("123456789*123456789");
  #  print &fortranify_line("123456789*123456789+123456789-123456789/123456789*123456789-123456789+123456789");
  #  print &fortranify_line("1234567890123456789012345678901234567890123456789012345678901234567890123456789");
  # 
  #
  my ($string)  = @_;

  my $tmpstring = '';
  my $fortran_string  = '';
  my $keep_wrapping   = 'no';
  my $position        = 0;
  my $wrap_position   = 0;

  my $test_char       = '';

  my $wrapat = {
      '+' => 1,
      '-' => 1,
      '/' => 1,
      '*' => 1,
      ' ' => 1 };

#                              1         2         3         4         5         6         7
#                     1234567890123456789012345678901234567890123456789012345678901234567890123456789
  my $new_pad      = '       ';
  my $continue_pad = '     c ';

  $tmpstring =  $new_pad.$string;

#print "         1         2         3         4         5         6         7 \n";
#print "1234567890123456789012345678901234567890123456789012345678901234567890123456789 \n";

  if(length($tmpstring) < 72){
    # wrapping is not necessary
    $fortran_string = $tmpstring."\n";
  } 
  else{
    # we need to wrap the lines
    $keep_wrapping = 'yes';
    while($keep_wrapping eq 'yes'){
      $position = 71;
      $wrap_position = -1;
      # now we start at position 72 and walk 
      # backwards through the string one character
      # at a time
      while($position > 6){
        $test_char = substr($tmpstring, $position, 1);
        # if the character is in our 'wrapat' structure
        # we store that position as the one where we wrap
        if(exists($wrapat->{$test_char})){
          $wrap_position = $position;
          $position = 0;
        }
        # otherwise we move to the next character
        else{
          $position = $position-1; }
      }

      # now we wrap WORD!
      if($wrap_position > 6){
        # peeling off the first part and adding it to the output
        $fortran_string .= substr($tmpstring, 0, $wrap_position)."\n";
         
        # appending the remainder to tmpstring
        $tmpstring = substr($tmpstring, ($wrap_position));

        # padding the first part with the continuation padding
        $tmpstring = $continue_pad.$tmpstring;
         
        # checking to see if the new tmpstring is below the 72
        # if it is we stop the loop
        if(length($tmpstring) < 72){
          $fortran_string .= $tmpstring."\n";
          $keep_wrapping = 'no';
        }
        # otherwise we continue
        
      
      }
      # we were unable to find a
      # position to wrap
      else{
        # we stop wrapping
        $keep_wrapping = 'no';
        # now we through up an error
        &mywarn("Failed to Fortran-ify the following line:"); 
        &mywarn($string); 
        &mywarn("You need to adjust the input file so that fewer");
        &mywarn("unwrappable characters occur in succession"); 
        }
      }
      $keep_wrapping = 'no';
    }
  
  return $fortran_string;
}

# use this function to parse user input in the form of:
# [ -1 2 3  23 42];
# and convert it into a list
sub extract_elements
{
 my ($tmp) = @_;
 my $string = $tmp;
 my @array;
 # stripping off the brackets
 $string =~ s#\[##g; 
 $string =~ s#\]##g;
 # removing any trailing spaces
 $string =~ s#^\s+##g;
 $string =~ s#\s+$##g;

 @array = split(/\s+/, $string);


 return @array;

}

sub apply_format
{
 my ($cfg, $string, $format) = @_;


 my $patterns;
 my $pattern;


# SIMINT_ARG_0  = Ctot
# SIMINT_ARG_1  = Rtot
# SIMINT_ARG_2  = KD   

# 0.5*((SIMINT_ARG_0 + SIMINT_ARG_1 + SIMINT_ARG_2)+SIMINT_POWER[SIMINT_POWER[(SIMINT_ARG_0 + SIMINT_ARG_1 + SIMINT_ARG_2)][2.0] + 4.0*SIMINT_ARG_0*SIMINT_ARG_2][0.5])

 $patterns->{qeq}->{pattern}                    =  'SIMINT_QEQ[';
 $patterns->{qeq}->{number_arguments}           =  3;
 $patterns->{qeq}->{C}                          =  '0.5*((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2) + pow((pow((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2),(2.0)) + 4.0*SIMINT_ARG_0*SIMINT_ARG_2),(0.5)))';
 $patterns->{qeq}->{matlab}                     =  '0.5*((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2) +         ((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2)^(2.0)  + 4.0*SIMINT_ARG_0*SIMINT_ARG_2)^(0.5))'; 
 $patterns->{qeq}->{pw}                         =  '0.5*((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2) +         ((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2)^(2.0)  + 4.0*SIMINT_ARG_0*SIMINT_ARG_2)^(0.5))'; 
 $patterns->{qeq}->{bm}                         =  '0.5*((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2) +         ((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2)^(2.0)  + 4.0*SIMINT_ARG_0*SIMINT_ARG_2)^(0.5))'; 
 $patterns->{qeq}->{fortran}                    =  '0.5*((SIMINT_ARG_0-SIMINT_ARG_1-SIMINT_ARG_2)+((SIMINT_ARG_0-SIMINT_ARG_1-SIMINT_ARG_2)**(2.0)+4.0*SIMINT_ARG_0*SIMINT_ARG_2)**(0.5))';
 $patterns->{qeq}->{monolix}                    =  '0.5*((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2) -         ((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2)^(2.0)  + 4.0*SIMINT_ARG_0*SIMINT_ARG_2)^(0.5))'; 
 $patterns->{qeq}->{nonmem}                     =  '0.5*((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2) -         ((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2)**(2.0) + 4.0*SIMINT_ARG_0*SIMINT_ARG_2)**(0.5))'; 
 $patterns->{qeq}->{rproject}                   =  '0.5*((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2) -         ((SIMINT_ARG_0 - SIMINT_ARG_1 - SIMINT_ARG_2)^(2.0)  + 4.0*SIMINT_ARG_0*SIMINT_ARG_2)^(0.5))'; 

 #my $occurance_index;

 $patterns->{power}->{pattern}                  =  'SIMINT_POWER[';
 $patterns->{power}->{number_arguments}         =  2;
 $patterns->{power}->{C}                        =  'pow((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{power}->{matlab}                   =  '(SIMINT_ARG_0)^(SIMINT_ARG_1)';
 $patterns->{power}->{pw}                       =  '(SIMINT_ARG_0)^(SIMINT_ARG_1)';
 $patterns->{power}->{bm}                       =  '(SIMINT_ARG_0)^(SIMINT_ARG_1)';
 $patterns->{power}->{fortran}                  =  '(SIMINT_ARG_0)**(SIMINT_ARG_1)';
 $patterns->{power}->{monolix}                  =  '(SIMINT_ARG_0)^(SIMINT_ARG_1)';
 $patterns->{power}->{nonmem}                   =  '(SIMINT_ARG_0)**(SIMINT_ARG_1)';
 $patterns->{power}->{rproject}                 =  '(SIMINT_ARG_0)^(SIMINT_ARG_1)';


 # error functions
 $patterns->{erf}->{pattern}                  =  'SIMINT_ERF[';
 $patterns->{erf}->{number_arguments}         =  1;
 $patterns->{erf}->{C}                        =  'erf(SIMINT_ARG_1)';
 $patterns->{erf}->{matlab}                   =  'erf(SIMINT_ARG_1)';
 $patterns->{erf}->{pw}                       =  'erf(SIMINT_ARG_1)';
 $patterns->{erf}->{bm}                       =  'erf(SIMINT_ARG_1)';
 $patterns->{erf}->{fortran}                  =  'ERF(SIMINT_ARG_1)';
 $patterns->{erf}->{monolix}                  =  'erf(SIMINT_ARG_1)';
 $patterns->{erf}->{nonmem}                   =  'ERF(SIMINT_ARG_1)';
 $patterns->{erf}->{rproject}                 =  'erf(SIMINT_ARG_1)';

 $patterns->{erfc}->{pattern}                  =  'SIMINT_ERFC[';
 $patterns->{erfc}->{number_arguments}         =  1;
 $patterns->{erfc}->{C}                        =  'erfc(SIMINT_ARG_1)';
 $patterns->{erfc}->{matlab}                   =  'erfc(SIMINT_ARG_1)';
 $patterns->{erfc}->{pw}                       =  'erfc(SIMINT_ARG_1)';
 $patterns->{erfc}->{bm}                       =  'erfc(SIMINT_ARG_1)';
 $patterns->{erfc}->{fortran}                  =  'ERFC(SIMINT_ARG_1)';
 $patterns->{erfc}->{monolix}                  =  'erfc(SIMINT_ARG_1)';
 $patterns->{erfc}->{nonmem}                   =  'ERFC(SIMINT_ARG_1)';
 $patterns->{erfc}->{rproject}                 =  'erfc(SIMINT_ARG_1)';

 # log and exp
 $patterns->{logn}->{pattern}                   =  'SIMINT_LOGN[';
 $patterns->{logn}->{number_arguments}          =  1;
 $patterns->{logn}->{C}                         =  'log(SIMINT_ARG_0)';
 $patterns->{logn}->{matlab}                    =  'log(SIMINT_ARG_0)';
 $patterns->{logn}->{pw}                        =  'log(SIMINT_ARG_0)';
 $patterns->{logn}->{bm}                        =  'LOGN(SIMINT_ARG_0)';
 $patterns->{logn}->{fortran}                   =  'LOG(SIMINT_ARG_0)';
 $patterns->{logn}->{monolix}                   =  'ln(SIMINT_ARG_0)';
 $patterns->{logn}->{nonmem}                    =  'LOG(SIMINT_ARG_0)';
 $patterns->{logn}->{rproject}                  =  'log(SIMINT_ARG_0)';


 $patterns->{log10}->{pattern}                  =  'SIMINT_LOG10[';
 $patterns->{log10}->{number_arguments}         =  1;
 $patterns->{log10}->{C}                        =  'log10(SIMINT_ARG_0)';
 $patterns->{log10}->{matlab}                   =  'log10(SIMINT_ARG_0)';
 $patterns->{log10}->{pw}                       =  'log10(SIMINT_ARG_0)';
 $patterns->{log10}->{bm}                       =  'LOG10(SIMINT_ARG_0)';
 $patterns->{log10}->{fortran}                  =  'LOG10(SIMINT_ARG_0)';
 $patterns->{log10}->{monolix}                  =  'log(SIMINT_ARG_0)';
 $patterns->{log10}->{nonmem}                   =  'LOG10(SIMINT_ARG_0)';
 $patterns->{log10}->{rproject}                 =  'log10(SIMINT_ARG_0)';

 $patterns->{exp}->{pattern}                    =  'SIMINT_EXP[';
 $patterns->{exp}->{number_arguments}           =  1;
 $patterns->{exp}->{C}                          =  'exp(SIMINT_ARG_0)';
 $patterns->{exp}->{matlab}                     =  'exp(SIMINT_ARG_0)';
 $patterns->{exp}->{pw}                         =  'exp(SIMINT_ARG_0)';
 $patterns->{exp}->{bm}                         =  'exp(SIMINT_ARG_0)';
 $patterns->{exp}->{fortran}                    =  'exp(SIMINT_ARG_0)';
 $patterns->{exp}->{monolix}                    =  'exp(SIMINT_ARG_0)';
 $patterns->{exp}->{nonmem}                     =  'exp(SIMINT_ARG_0)';
 $patterns->{exp}->{rproject}                   =  'exp(SIMINT_ARG_0)';


 $patterns->{SEQ}->{pattern}                    =  'SIMINT_SEQ[';
 $patterns->{SEQ}->{number_arguments}           =  3;
 $patterns->{SEQ}->{C}                          =  'NO_FUNC(SIMINT_ARG_0, SIMINT_ARG_1, SIMINT_ARG_2)';
 $patterns->{SEQ}->{matlab}                     =  'linspace(SIMINT_ARG_0, SIMINT_ARG_1, (SIMINT_ARG_1-SIMINT_ARG_0)/SIMINT_ARG_2)';
 $patterns->{SEQ}->{pw}                         =  'linspace(SIMINT_ARG_0, SIMINT_ARG_1, (SIMINT_ARG_1-SIMINT_ARG_0)/SIMINT_ARG_2)';
 $patterns->{SEQ}->{bm}                         =  'NO_FUNC(SIMINT_ARG_0, SIMINT_ARG_1, SIMINT_ARG_2)';
 $patterns->{SEQ}->{fortran}                    =  'NO_FUNC(SIMINT_ARG_0, SIMINT_ARG_1, SIMINT_ARG_2)';
 $patterns->{SEQ}->{monolix}                    =  'linspace(SIMINT_ARG_0, SIMINT_ARG_1, (SIMINT_ARG_1-SIMINT_ARG_0)/SIMINT_ARG_2)';
 $patterns->{SEQ}->{nonmem}                     =  'NO_FUNC(SIMINT_ARG_0, SIMINT_ARG_1, SIMINT_ARG_2)';
 $patterns->{SEQ}->{rproject}                   =  'seq(SIMINT_ARG_0, SIMINT_ARG_1, SIMINT_ARG_2)';

#$patterns->{?}->{pattern}                  =  ;# 'SIMINT_[';
#$patterns->{?}->{number_arguments}         =  ;# 2;
#$patterns->{?}->{C}                        =  ;#'(SIMINT_ARG_0) (SIMINT_ARG_1)';
#$patterns->{?}->{matlab}                   =  ;#'(SIMINT_ARG_0) (SIMINT_ARG_1)';
#$patterns->{?}->{pw}                       =  ;#'(SIMINT_ARG_0) (SIMINT_ARG_1)';
#$patterns->{?}->{bm}                       =  ;#'(SIMINT_ARG_0) (SIMINT_ARG_1)';
  
  
 #
 # Boolean and logical operators
 #
 $patterns->{LT}->{pattern}                  =  'SIMINT_LT[';   
 $patterns->{LT}->{number_arguments}         =   2;
 $patterns->{LT}->{C}                        =  '(SIMINT_ARG_0) <  (SIMINT_ARG_1)';
 $patterns->{LT}->{matlab}                   =  '(SIMINT_ARG_0) <  (SIMINT_ARG_1)';
 $patterns->{LT}->{pw}                       =  'lt((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{LT}->{bm}                       =  '(SIMINT_ARG_0) < (SIMINT_ARG_1)';
 $patterns->{LT}->{fortran}                  =  '(SIMINT_ARG_0).LT.(SIMINT_ARG_1)';
 $patterns->{LT}->{monolix}                  =  '(SIMINT_ARG_0) <  (SIMINT_ARG_1)';
 $patterns->{LT}->{nonmem}                   =  '(SIMINT_ARG_0).LT.(SIMINT_ARG_1)';
 $patterns->{LT}->{rproject}                 =  '(SIMINT_ARG_0) <  (SIMINT_ARG_1)';


 $patterns->{LE}->{pattern}                  =   'SIMINT_LE[';   
 $patterns->{LE}->{number_arguments}         =   2;
 $patterns->{LE}->{C}                        =  '(SIMINT_ARG_0) <= (SIMINT_ARG_1)';
 $patterns->{LE}->{matlab}                   =  '(SIMINT_ARG_0) <= (SIMINT_ARG_1)';
 $patterns->{LE}->{pw}                       =  'le((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{LE}->{bm}                       =  '(SIMINT_ARG_0) <= (SIMINT_ARG_1)';
 $patterns->{LE}->{fortran}                  =  '(SIMINT_ARG_0).LE.(SIMINT_ARG_1)';
 $patterns->{LE}->{monolix}                  =  '(SIMINT_ARG_0) <= (SIMINT_ARG_1)';
 $patterns->{LE}->{nonmem}                   =  '(SIMINT_ARG_0).LE.(SIMINT_ARG_1)';
 $patterns->{LE}->{rproject}                 =  '(SIMINT_ARG_0) <= (SIMINT_ARG_1)';

 $patterns->{GT}->{pattern}                  =   'SIMINT_GT[';   
 $patterns->{GT}->{number_arguments}         =   2;
 $patterns->{GT}->{C}                        =  '(SIMINT_ARG_0) >  (SIMINT_ARG_1)';
 $patterns->{GT}->{matlab}                   =  '(SIMINT_ARG_0) >  (SIMINT_ARG_1)';
 $patterns->{GT}->{pw}                       =  'gt((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{GT}->{bm}                       =  '(SIMINT_ARG_0) >  (SIMINT_ARG_1)';
 $patterns->{GT}->{fortran}                  =  '(SIMINT_ARG_0).GT.(SIMINT_ARG_1)';
 $patterns->{GT}->{monolix}                  =  '(SIMINT_ARG_0) >  (SIMINT_ARG_1)';
 $patterns->{GT}->{nonmem}                   =  '(SIMINT_ARG_0).GT.(SIMINT_ARG_1)';
 $patterns->{GT}->{rproject}                 =  '(SIMINT_ARG_0) >  (SIMINT_ARG_1)';

 $patterns->{GE}->{pattern}                  =   'SIMINT_GE[';   
 $patterns->{GE}->{number_arguments}         =   2;
 $patterns->{GE}->{C}                        =  '(SIMINT_ARG_0) >= (SIMINT_ARG_1)';
 $patterns->{GE}->{matlab}                   =  '(SIMINT_ARG_0) >= (SIMINT_ARG_1)';
 $patterns->{GE}->{pw}                       =  'ge((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{GE}->{bm}                       =  '(SIMINT_ARG_0) >= (SIMINT_ARG_1)';
 $patterns->{GE}->{fortran}                  =  '(SIMINT_ARG_0).GE.(SIMINT_ARG_1)';
 $patterns->{GE}->{monolix}                  =  '(SIMINT_ARG_0) >= (SIMINT_ARG_1)';
 $patterns->{GE}->{nonmem}                   =  '(SIMINT_ARG_0).GE.(SIMINT_ARG_1)';
 $patterns->{GE}->{rproject}                 =  '(SIMINT_ARG_0) >= (SIMINT_ARG_1)';

 $patterns->{EQ}->{pattern}                  =   'SIMINT_EQ[';   
 $patterns->{EQ}->{number_arguments}         =   2;
 $patterns->{EQ}->{C}                        =  '(SIMINT_ARG_0) == (SIMINT_ARG_1)';
 $patterns->{EQ}->{matlab}                   =  '(SIMINT_ARG_0) == (SIMINT_ARG_1)';
 $patterns->{EQ}->{pw}                       =  'eq((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{EQ}->{bm}                       =  '(SIMINT_ARG_0) =  (SIMINT_ARG_1)';
 $patterns->{EQ}->{fortran}                  =  '(SIMINT_ARG_0).EQ.(SIMINT_ARG_1)';
 $patterns->{EQ}->{monolix}                  =  '(SIMINT_ARG_0) == (SIMINT_ARG_1)';
 $patterns->{EQ}->{nonmem}                   =  '(SIMINT_ARG_0).EQ.(SIMINT_ARG_1)';
 $patterns->{EQ}->{rproject}                 =  '(SIMINT_ARG_0) == (SIMINT_ARG_1)';


 $patterns->{NE}->{pattern}                  =   'SIMINT_NE[';   
 $patterns->{NE}->{number_arguments}         =   2;
 $patterns->{NE}->{C}                        =  '(SIMINT_ARG_0) != (SIMINT_ARG_1)';
 $patterns->{NE}->{matlab}                   =  '(SIMINT_ARG_0) ~= (SIMINT_ARG_1)';
 $patterns->{NE}->{pw}                       = '' ;#'(SIMINT_ARG_0) (SIMINT_ARG_1)'; #JMH no pw?
 $patterns->{NE}->{bm}                       =   '(SIMINT_ARG_0) (SIMINT_ARG_1)';
 $patterns->{NE}->{fortran}                  =   '(SIMINT_ARG_0).NE.(SIMINT_ARG_1)';
 $patterns->{NE}->{monolix}                  =  '(SIMINT_ARG_0) ~= (SIMINT_ARG_1)';
 $patterns->{NE}->{nonmem}                   =   '(SIMINT_ARG_0).NE.(SIMINT_ARG_1)';
 $patterns->{NE}->{rproject}                 =  '(SIMINT_ARG_0) != (SIMINT_ARG_1)';

 $patterns->{OR}->{pattern}                  =   'SIMINT_OR[';   
 $patterns->{OR}->{number_arguments}         =   2;
 $patterns->{OR}->{C}                        =  '(SIMINT_ARG_0) || (SIMINT_ARG_1)';
 $patterns->{OR}->{matlab}                   =  '(SIMINT_ARG_0) || (SIMINT_ARG_1)';
 $patterns->{OR}->{pw}                       =  'or((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{OR}->{bm}                       =  '(SIMINT_ARG_0) OR (SIMINT_ARG_1)';
 $patterns->{OR}->{fortran}                  =  '(SIMINT_ARG_0).OR.(SIMINT_ARG_1)';
 $patterns->{OR}->{monolix}                  =  '(SIMINT_ARG_0) |  (SIMINT_ARG_1)';
 $patterns->{OR}->{nonmem}                   =  '(SIMINT_ARG_0).OR.(SIMINT_ARG_1)';
 $patterns->{OR}->{rproject}                 =  '(SIMINT_ARG_0) || (SIMINT_ARG_1)';

 $patterns->{AND}->{pattern}                  =   'SIMINT_AND[';   
 $patterns->{AND}->{number_arguments}         =   2;
 $patterns->{AND}->{C}                        =  '(SIMINT_ARG_0) && (SIMINT_ARG_1)';
 $patterns->{AND}->{matlab}                   =  '(SIMINT_ARG_0) &  (SIMINT_ARG_1)';
 $patterns->{AND}->{pw}                       =  'and((SIMINT_ARG_0),(SIMINT_ARG_1))';
 $patterns->{AND}->{bm}                       =  '(SIMINT_ARG_0) AND (SIMINT_ARG_1)';
 $patterns->{AND}->{fortran}                  =  '(SIMINT_ARG_0).AND.(SIMINT_ARG_1)';
 $patterns->{AND}->{monolix}                  =  '(SIMINT_ARG_0) &  (SIMINT_ARG_1)';
 $patterns->{AND}->{nonmem}                   =  '(SIMINT_ARG_0).AND.(SIMINT_ARG_1)';
 $patterns->{AND}->{rproject}                 =  '(SIMINT_ARG_0) && (SIMINT_ARG_1)';

 $patterns->{NOT}->{pattern}                  =   'SIMINT_NOT[';   
 $patterns->{NOT}->{number_arguments}         =   1;
 $patterns->{NOT}->{C}                        =  '!(SIMINT_ARG_0)';
 $patterns->{NOT}->{matlab}                   =  'not(SIMINT_ARG_0)';
 $patterns->{NOT}->{pw}                       = '' ;#'(SIMINT_ARG_0)'; #JMH no pw?
 $patterns->{NOT}->{bm}                       =   'NOT  SIMINT_ARG_0 ';
 $patterns->{NOT}->{fortran}                  =   '.NOT.SIMINT_ARG_0';
 $patterns->{NOT}->{monolix}                  =  '~(SIMINT_ARG_0)';
 $patterns->{NOT}->{nonmem}                   =   '.NOT.SIMINT_ARG_0';
 $patterns->{NOT}->{matlab}                   =  '(!SIMINT_ARG_0)';


 # if we're working with nonmem we need to first amtify 
 # any states that need amtifying 
 if($format eq 'nonmem'){
   # so we check to see if any amtify states have been specified
   if(scalar(keys(%{$cfg->{options}->{amtify}->{cmt_to_rel}}))>0){
     $string = &remap_namespace($string , 
                                $cfg->{options}->{amtify}->{cmt_to_rel});
   }
 }

 foreach $pattern (keys(%{$patterns})){
   while(index($string, $patterns->{$pattern}->{pattern})>-1){
    $string = &apply_function($string, 
                              $patterns->{$pattern}->{pattern},
                              $patterns->{$pattern}->{number_arguments},
                              $patterns->{$pattern}->{$format});

   }
 }

 return $string;

}

sub parse_nonmem_options
{
  my ($cfg, $line) = @_;

  $line =~ s#\s##g;

  my $col;
  my $value;

  if($line =~ m#<NONMEM:INPUT:DROP:#){
     $line =~ s#<NONMEM:INPUT:DROP:(.*)>.*#$1#;
     $cfg->{options}->{nonmem}->{input}->{drop}->{$line} = 'yes';
  }
  if($line =~ m#<NONMEM:INPUT:RENAME#){
     $col   = $line;
     $value = $line;
     $col   =~ s#<NONMEM:INPUT:RENAME:(.*)>.*#$1#;
     $value =~ s#<NONMEM:INPUT:RENAME:.*>(.*)#$1#;
     $cfg->{options}->{nonmem}->{input}->{rename}->{$col} = $value;
  }
  if($line =~ m#<NONMEM:DATA>#){
     $line =~ s#<NONMEM:DATA>\s*##;
     $cfg->{options}->{nonmem}->{data} .= $line."\n";
  }
  return $cfg;
}

sub parse_amtify
{
  my ($cfg, $line) = @_;

  my $value;

  $value = $line;
  $value =~ s#\s##g;

  my ($conc, $amt, $rel);

  if($line =~ m#<AMTIFY>#){
     # Stripping off the delimiter
     $value =~ s#<AMTIFY>##;
     # pulling out the different components
     my @elements = split(';', $value);
     if(scalar(@elements) == 3){
       ($conc, $amt, $rel) = @elements;
       $cfg->{options}->{amtify}->{cmt_to_amt}->{$conc} = $amt;
       $cfg->{options}->{amtify}->{cmt_to_rel}->{$conc} = "($rel)";
     }
     else{
       &mywarn("AMTIFY: Unable to parse");
       &mywarn($line); 
       }
  }
  return $cfg;
}


sub parse_data_file
{
  my ($cfg, $line) = @_;

  # the file name has been specified
  if($line =~ m#<DATA:FILE:CSV>#){
    $line  =~ s#<DATA:FILE:CSV>\s*##;
    $cfg->{data}->{file} = $line;
  }

  # attempting to find the header information
  if($line =~ m#<DATA:HEADER:AUTOMATIC>#){
    $cfg->{data}->{headers}->{mode} = 'automatic'; }
  elsif($line =~ m#<DATA:HEADER:MANUAL>#){
    $cfg->{data}->{headers}->{mode} = 'manual';

    # stripping out the descriptor and any spaces
    $line   =~ s#<DATA:HEADER:MANUAL>\s*##;
    $line   =~ s#\s##g;
    push @{$cfg->{data}->{headers}->{values}},  split(';', $line);
  }



  return $cfg;
}


sub parse_guide
{
  my ($cfg, $line) = @_;


  $line   =~ s#<GUIDE>\s*##;
  my @chunks =  split('\s*;\s*', $line);

  my $gtype   = $chunks[0]; 
  my $gname   = $chunks[1]; 
  my $glevel  = $chunks[2]; 
  my $gmarker = $chunks[3]; 
  my $gcolor  = $chunks[4]; 


  $cfg->{guides}->{$gname} = {
     type     => $gtype,
     level    => $glevel,
     marker   => $gmarker,
     color    => $gcolor };


  return $cfg;
}

#---------------------------------------------
sub update_order
{
  my ($cfg, $line) = @_;

  my $type;
  my $value;
  my $index_string;
  my @new_index;
  my @old_index;
  my $cntr;

  foreach $type (keys(%{$cfg->{index}})){
    # mapping type specification in the index delimiter
    # to the index string used internally
    #  STATE = species_index
    if($type eq 'STATE'){
      $index_string = 'species_index'; }
    elsif($type eq 'OUTPUT'){
      $index_string = 'outputs_index'; }

    # if there are indices specified for the 
    # current $type then we process those
    if((%{$cfg->{index}->{$type}->{byname}})){
      @new_index = ();
      @old_index = @{$cfg->{$index_string}};



      # populating those index values specified by the user
      foreach $value (keys(%{$cfg->{index}->{$type}->{byvalue}})){
        $new_index[$value-1] = $cfg->{index}->{$type}->{byvalue}->{$value};
      }
       
      while ($value = shift(@old_index)){
        # first we check to see if the value 
        # was specified by the user
        if(not(defined($cfg->{index}->{$type}->{byname}->{$value}))){
          # if it wasn't specified by the user we put the value
          # in the first undefined element in the @new_index
          for($cntr=0; $cntr < scalar(@{$cfg->{$index_string}}); $cntr++){
            if(not(defined($new_index[$cntr]))){
              $new_index[$cntr] = $value;
              # this breaks out of the for loop
              $cntr = scalar(@{$cfg->{$index_string}}) + 1;
            }
          }
        }
      }


      # now replacing the index in $cfg with
      # the reordered index:
      $cfg->{$index_string} = ();
      push @{$cfg->{$index_string}}, @new_index;
    }
    
  }


  return $cfg;
}

#---------------------------------------------


sub parse_index
{
  my ($cfg, $line) = @_;


  if($line =~ m#<INDEX:(\S+):\S+>\s+\d+.*#){
    my $type  = $line;
    my $name  = $line;
    my $value = $line;
    
    $type  =~ s#<INDEX:(\S+):\S+>\s+\d+.*#$1#;
    $name  =~ s#<INDEX:\S+:(\S+)>\s+\d+.*#$1#;
    $value =~ s#<INDEX:\S+:\S+>\s+(\d+).*#$1#;

    if(($type eq 'STATE') or ($type eq 'OUTPUT')){
      # creating the data structure for index
      # these are organized by name, by value
      # 
      if(not(exists($cfg->{index}->{$type}->{byvalue}->{$value}))){
        $cfg->{index}->{$type}->{byname}->{$name}   = $value;
        $cfg->{index}->{$type}->{byvalue}->{$value} = $name;
        }
      else{
        &mywarn("Another $type with index $value has been specified"); 
        &mywarn("The line below has been ignored:"); 
        &mywarn($line);
        }
    }
    else{
      &mywarn("Unable to identify the TYPE:"); 
      &mywarn($line);
      &mywarn("Expected: <INDEX:TYPE:NAME> VALUE"); 
      }

    }
    else{
     &mywarn("Unable to intrepret the following <INDEX> specification"); 
     &mywarn($line); }



  return $cfg;
}

sub parse_option
{
  my ($cfg, $line) = @_;
  # line contains OPT
  my $name  = $line;
  my $value = $line;

  $name  =~ s#<OPT:(\S+)>\s+\S+.*#$1#;
  $value =~ s#<OPT:\S+>\s+(\S+.*)#$1#;

  $cfg->{options}->{$name} = $value;

  return $cfg;
}


sub find_bracketed_arguments
{

 my ($test_string, $start_pattern, $num_arguments, $new_pattern) = @_;
 #
 #
 #  $test_string     = 'SIMINT_POWER[SIMINT_EXP[SIMINT_POWER[c][c]]][w]';
 #  $start_pattern   = 'SIMINT_POWER[';
 #  $num_arguments   = 2;

 my $str_idx;
 my $argument_idx;


 my $result = {};
 
 $result->{arguments}      = ();
 $result->{function_start} = '';
 $result->{function_stop} = '';
 $result->{error}         = 0;
 $result->{argument_end}  = 0;


 # finding where the function begins and ends
 $result->{function_start}  = index($test_string, $start_pattern);
 $result->{function_stop}   = length($start_pattern) + $result->{function_start};
 my $argument_offset = 0;
 my $bracket_counter = 1;
 my $letter;


 # finding the arguments for the function:
 for ($argument_idx =1; $argument_idx <= $num_arguments; $argument_idx ++){
  if($argument_idx eq 1){
    $argument_offset = $result->{function_stop}; }
  $bracket_counter = 1;
  if($result->{error} <1){
    for (my $str_idx = $argument_offset; $str_idx < length($test_string); $str_idx++) {
      $letter = substr ($test_string, $str_idx, 1 );
     
      # here we found an open bracket so we increment the counter
      if($letter eq '['){
          $bracket_counter = $bracket_counter + 1; }
      # here we found an closed bracket so we decrement the counter
      # and if the counter is at 0 then we've found the match we're looking for
      if($letter eq ']'){
          $bracket_counter = $bracket_counter -1;
          if($bracket_counter == 0){
            # storing the argument
            push @{$result->{arguments}}, substr($test_string, $argument_offset, ($str_idx-$argument_offset));
            # updating the argument offset
            # storing the end of the argument (for the last argument 
            # this will mark the end of the entire function call) 
            $result->{argument_end}    = $str_idx+1;
            # adjusting the offset
            $argument_offset = index($test_string, '[', $str_idx)+1;
            # breaking out of for loop
            $str_idx = length($test_string) + 1;
          }
        }
     
       # if we made it here, then we didn't find the matching bracket for the current
       # argument
       if($str_idx eq (length($test_string) -1)){
         print "Failed to match brackets on pattern: $start_pattern \n";
         print "line: $test_string \n";
         print "No longer parsing this line \n";
         $result->{error} = 1;
       }
     }
   }
 }

 return $result;
}

sub apply_set_functions
{
    my ($cfg, $line) = @_;

    my $patterns;
    my $set_type;
    my $set_operator;
    my $set_element;
    my $set_name   ;
    my $function_components;

    # function elements
    my @elements;
    my $element;


    $patterns->{sum}->{pattern}  = 'SIMINT_SET_SUM[';
    $patterns->{sum}->{operator} = '+';

    $patterns->{product}->{pattern}  = 'SIMINT_SET_PRODUCT[';
    $patterns->{product}->{operator} = '*';

    #initializing the error to 0, it will be overwritten with subsequent iterations
    $function_components->{error}    = 0;

    #$patterns->{prod} = 'SIMINT_SET_PROD[';
    #

    foreach $set_type (keys(%{$patterns})){
      while(index($line, $patterns->{$set_type}->{pattern})>-1 and ($function_components->{error} <1) ){
        $set_operator = $patterns->{$set_type}->{operator};
        $function_components = &find_bracketed_arguments($line, $patterns->{$set_type}->{pattern}, 2);
        # checking to make sure the function parsed correctly
        if($function_components->{error} < 1){
          $set_name = $function_components->{arguments}->[0];
          if(defined($cfg->{sets}->{$set_name})){
            # empting the elements of the set
            @elements = ();
            foreach $set_element (@{$cfg->{sets}->{$set_name}}){
              $element = "(".$function_components->{arguments}->[1].")";
              $element =~ s#{$set_name}#$set_element#g;
              push @elements, $element;
            }
            substr($line, 
                   $function_components->{function_start}, 
                  ($function_components->{argument_end}-$function_components->{function_start}), 
                  "(".join("$set_operator", @elements).")");
          }
          else{
            &mywarn("Set name not recognized ");
            &mywarn("$patterns->{$set_type}->{pattern}$function_components->{arguments}->[0]]");
          }
        }
      }
    }


    return $line;
}




sub apply_sets
{
    my ($cfg, $line) = @_;
    my @lines     = ($line);
    my @tmp_lines = ();
    my $tmp_line;
    my @set_names;
    
    # getting a list of the sets
    push @set_names, keys(%{$cfg->{sets}});

    my ($set_name, $set_element);

    # for each set we see if it is present in each 
    # $line of @lines
    #
    # if it is present in that line then we create 
    # a new line for each possible
    # element in the set


    foreach $set_name (@set_names){
      # tmp_lines acts like a stack to 
      # hold on to each line that's processed
      @tmp_lines = ();
      foreach $line (@lines){
       if($line =~ m#{$set_name}#){
         foreach $set_element (@{$cfg->{sets}->{$set_name}}){
           $tmp_line = $line;
           $tmp_line =~ s#{$set_name}#$set_element#g;
           push @tmp_lines, $tmp_line;
         }
       }
       # if it doesn't have this set, just a different one
       # then we add that line to tmp_lines
       else{
         push @tmp_lines, $line;
       }
      }
      @lines = @tmp_lines;
    }
    
    return @lines;
}

sub parse_set
{
  my ($cfg, $line) = @_;
  if($line =~ '<SET:\S+>\s*\S+'){
    my $set_name       = $line;
    my $element_string = $line;
    # pulling out the setname and string of sets
    $set_name          =~ s#\s*<SET:\s*(\S+)\s*>\s*\S+.+#$1#;
    $element_string    =~ s#\s*<SET:\s*\S+\s*>\s*(\S+.+)\s*#$1#;

    # adding the elements of the set
    push @{$cfg->{sets}->{$set_name}}, split(/\s*;\s*/, $element_string);

  }

  return $cfg;
}



sub parse_ode
{
  my ($cfg, $line) = @_;
  # line contains odes
  if($line =~ '<ODE:\S+>\s*\S+'){
    my $species = $line;
    my $ode     = $line;

    # pulling out the species and ode
    $species  =~ s#\s*<ODE:\s*(\S+)\s*>\s*\S+.*#$1#;
    $ode      =~ s#\s*<ODE:\s*\S+\s*>\s*(\S+.*)#$1#;

    # making sure the species has been initialized 
    $cfg = &initialize_species($cfg, $species); 
     push @{$cfg->{species}->{$species}->{odes}}, "($ode)"; 

  }


  return $cfg;
}



sub parse_time_scales
{
    my ($cfg, $line) = @_;

    if($line =~ '<TS:\S+>\s+\S+'){
      my $scale_name = $line;
      $scale_name =~ s#<TS:(\S+)>\s+\S+#$1#;

      my $scale_value = $line;
      $scale_value =~ s#<TS:\S+>\s+(\S+)#$1#;

      push @{$cfg->{time_scales_index}}, $scale_name; 
      $cfg->{time_scales}->{$scale_name} = $scale_value;
    }

    return $cfg;

}

sub parse_output
{
    my ($cfg, $line) = @_;

    if($line =~ '<O>\s*\S+\s*=.*'){
      # stripping off output label
      $line =~ s#<O>\s*##;

      my $oname = $line;
      $oname =~ s#\s*(\S+)\s*=.*#$1#;

      my $ovalue = $line;
      $ovalue =~ s#\s*\S+\s*=\s*(.*)#$1#;

      #print ">$oname<\n";
      #print ">$ovalue<\n";
      if(defined($cfg->{outputs}->{$oname})){
        &mywarn("Output -->$oname<-- defined multiple times, skipping subsequent entries");}
      else{
        $cfg->{outputs}->{$oname} = $ovalue;
        push @{$cfg->{outputs_index}}, $oname; 
      }

    }
    else{
      &mywarn("output entry found but format is unexpected" ); 
      &mywarn("entry: -->$line<--"); 
  }
    return $cfg;
}

sub parse_bolus_inputs
{
    my ($cfg, $line) = @_;

    my @chunks =  split('\s*;\s*', $line);
    # we've found the time(s) of the 
    # bolus events
    if($line=~ m/<B:times>/){
      $cfg->{bolus_inputs}->{times}->{values} = $chunks[1]; 
      $cfg->{bolus_inputs}->{times}->{scale}  = $chunks[2]; 
      $cfg->{bolus_inputs}->{times}->{units}  = $chunks[3];

      # removing commas
      $cfg->{bolus_inputs}->{times}->{values} =~ s#,# #g;
      # stripping out extra spaces
      $cfg->{bolus_inputs}->{times}->{values} =~ s#\s+# #g;
    }


    if($line=~ m#<B:events>#){
      my $state = $chunks[1];
      $cfg->{bolus_inputs}->{entries}->{$state}->{values} = $chunks[2];
      $cfg->{bolus_inputs}->{entries}->{$state}->{scale}  = $chunks[3];
      $cfg->{bolus_inputs}->{entries}->{$state}->{units}  = $chunks[4];

      # removing commas
      $cfg->{bolus_inputs}->{entries}->{$state}->{values} =~ s#,# #g;
      # stripping out extra spaces
      $cfg->{bolus_inputs}->{entries}->{$state}->{values} =~ s#\s+# #g;
    }

    return $cfg;
}

sub parse_interindivudal_varability
{
    my ($cfg, $line) = @_;

    # removing any spaces
    $line =~ s#\s+##g;
    # The default IIVSET is 'default'
    my $IIV_SET                = 'default'; 
    # if an alternative has been specified then we 
    # pull it out and reduce the line to a normal iiv 
    # call
    if($line=~ m#^<IIVSET#){
      $IIV_SET      = $line;
      $IIV_SET      =~ s#<IIVSET:(\S+?):.*#$1#g; 
      $line         =~ s#SET:$IIV_SET##g; 
    }
    if($line=~ m#^<IIVCORSET#){
      $IIV_SET      = $line;
      $IIV_SET      =~ s#<IIVCORSET:(\S+?):.*#$1#g; 
      $line         =~ s#SET:$IIV_SET##g; 
    }

    # these will be trimmed down below according to the 
    # <IIV descriptor used
    my $IIV_NAME               = $line;
    my $IIV_NAME2              = $line;
    my $IIV_VALUE              = $line;
    my $IIV_PARAMETER          = $line;
    my $IIV_PARAMETER_OTHER    = $line;
    my $IIV_DISTRIBUTION       = $line;

    # IIV parameter association
    if($line=~ m#<IIV:\S+:\S+>\S*#){
      $IIV_NAME         =~ s#<IIV:(\S+):\S+>\S*#$1#g; 
      $IIV_PARAMETER    =~ s#<IIV:\S+:\S+>(\S*)#$1#g; 
      $IIV_DISTRIBUTION =~ s#<IIV:\S+:(\S+)>\S*#$1#g; 
      $cfg = &initialize_iiv($IIV_NAME, $cfg, $IIV_SET);

      if($IIV_DISTRIBUTION eq 'WB'){
         $IIV_PARAMETER_OTHER    =~ s#<IIV:\S+:\S+>(\S*)#$1#g; 
         $IIV_PARAMETER_OTHER    =~ s#\S+;(\S+;\S+)#$1#;
         $IIV_PARAMETER          =~ s#(\S);\S*#$1#;
         #
         # IIV_PARAMETER_OTHER contains the Weibull parameters
         #  - First entry:  K - Shape parameter  (>0)
         #  - Second entry: L - Scale parameter  (>0)
         @{$cfg->{iiv}->{$IIV_SET}->{parameters}->{$IIV_PARAMETER}->{other_parameters}}  = split(';', $IIV_PARAMETER_OTHER);
      }
      if($IIV_DISTRIBUTION eq 'BC'){
         $IIV_PARAMETER_OTHER    =~ s#<IIV:\S+:\S+>(\S*)#$1#g; 
         $IIV_PARAMETER_OTHER    =~ s#\S+;(\S+;\S+)#$1#;
         $IIV_PARAMETER          =~ s#(\S);\S*#$1#;
         #
         # IIV_PARAMETER_OTHER contains the Weibull parameters
         #  - First entry:  K - Shape parameter  (>0)
         #  - Second entry: L - Scale parameter  (>0)
         @{$cfg->{iiv}->{$IIV_SET}->{parameters}->{$IIV_PARAMETER}->{other_parameters}}  = split(';', $IIV_PARAMETER_OTHER);
      }


      $cfg->{iiv}->{$IIV_SET}->{parameters}->{$IIV_PARAMETER}->{iiv_name}     = $IIV_NAME;
      $cfg->{iiv}->{$IIV_SET}->{parameters}->{$IIV_PARAMETER}->{distribution} = $IIV_DISTRIBUTION;
      push @{$cfg->{iiv}->{$IIV_SET}->{iivs}->{$IIV_NAME}->{parameters}}, $IIV_PARAMETER;
    }
    # IIV value declaration
    elsif($line=~ m#<IIV:\S+>\S#){
      $IIV_NAME      =~ s#<IIV:(\S+)>\S*#$1#g; 
      $IIV_VALUE     =~ s#<IIV:\S+>(\S*)#$1#g; 
      $cfg = &initialize_iiv($IIV_NAME, $cfg, $IIV_SET);
      $cfg->{iiv}->{$IIV_SET}->{vcv}->{$IIV_NAME}->{$IIV_NAME} = $IIV_VALUE;
    }
    # IIV value declaration
    elsif($line=~ m#<IIVCOR:\S+:\S+>\S+#){
      $IIV_NAME      =~ s#<IIVCOR:(\S+):\S+>\S+#$1#g; 
      $IIV_NAME2     =~ s#<IIVCOR:\S+:(\S+)>\S+#$1#g; 
      $IIV_VALUE     =~ s#<IIVCOR:\S+:\S+>(\S+)#$1#g; 
      $cfg = &initialize_iiv($IIV_NAME, $cfg, $IIV_SET);
      $cfg = &initialize_iiv($IIV_NAME2, $cfg, $IIV_SET);
      $cfg->{iiv}->{$IIV_SET}->{vcv}->{$IIV_NAME}->{$IIV_NAME2} = $IIV_VALUE;
      $cfg->{iiv}->{$IIV_SET}->{vcv}->{$IIV_NAME2}->{$IIV_NAME} = $IIV_VALUE;
    }
    else{
     &mywarn("Unable to parse the following IIV declaration: ");
     &mywarn("$line");}

    return $cfg;
}

sub initialize_iiv
{
    my ($iiv_name, $cfg, $iiv_set) = @_;
    # if the IIV hasn't been defined below we construct an
    # empty data structure for it
    if(not(defined( $cfg->{iiv}->{$iiv_set}->{iivs}->{$iiv_name}))){
      $cfg->{iiv}->{$iiv_set}->{iivs}->{$iiv_name}->{parameters} = ();
      push @{$cfg->{iiv_index}->{$iiv_set}}, $iiv_name; 
    }

    return $cfg;
}



sub parse_input_covariate    
{
    my ($cfg, $line) = @_;
    my @chunks = split('\s*;\s*', $line);

    my $input_name = '';
    my $pset_name  = '';
    my $data_type  = '';
    my $cv_type    = '';


    # covariate defined
    if(($line=~ m#\s*<CV:\S+>.*#) and 
       (scalar(@chunks) eq 4)){
      # we've encountered a covariate definition
      $input_name = $chunks[0];
      $input_name =~ s/\s*<CV:(\S+)>.*/$1/;
      $cv_type    = 'linear';

      # if we haven't encountered this covariate before, 
      # we add it to the list of covariates 
      if(not(defined($cfg->{covariates}->{$input_name}))){
        push @{$cfg->{covariates_index}}, $input_name;}
      $data_type = $chunks[1];
      $cfg->{covariates}->{$input_name}->{cv_type}    = 'linear';
      $cfg->{covariates}->{$input_name}->{$data_type}->{units}   = $chunks[3];
      $cfg->{covariates}->{$input_name}->{parameter_sets}->{default}->{$data_type} = $chunks[2];

      # removing commas
      $cfg->{covariates}->{$input_name}->{parameter_sets}->{default}->{$data_type} =~ s#,# #g;
      # stripping out extra spaces
      $cfg->{covariates}->{$input_name}->{parameter_sets}->{default}->{$data_type} =~ s#\s+# #g;
    }
    # different covariate for a specific parameter set 
    elsif(($line=~ m#\s*<CVSET:\S+:\S+>.*#) and 
          (scalar(@chunks) eq 3)){
      $input_name = $chunks[0];
      $pset_name  = $chunks[0];
      $input_name =~ s/\s*<CVSET:\S+:(\S+)>.*/$1/;
      $pset_name  =~ s/\s*<CVSET:(\S+):\S+>.*/$1/;
      $data_type = $chunks[1];
      $cfg->{covariates}->{$input_name}->{parameter_sets}->{$pset_name}->{$data_type} = $chunks[2];
      # removing commas
      $cfg->{covariates}->{$input_name}->{parameter_sets}->{$pset_name}->{$data_type} =~ s#,# #g;
      # stripping out extra spaces
      $cfg->{covariates}->{$input_name}->{parameter_sets}->{$pset_name}->{$data_type} =~ s#\s+# #g;
    }
    elsif(($line=~ m#\s*<CVTYPE:\S+>#) ){
      $input_name = $chunks[0];
      $input_name =~ s/\s*<CVTYPE:(\S+)>.*/$1/;
      if(($line=~ m#\s*<CVTYPE:\S+>\s*linear#) ){
        $cv_type    = 'linear';}
      elsif(($line=~ m#\s*<CVTYPE:\S+>\s*step#) ){
        $cv_type    = 'step'; }
        $cfg->{covariates}->{$input_name}->{cv_type}     = $cv_type;
    }
    else{
    print scalar(@chunks);
      &mywarn("Unable to process the following model");
      &mywarn("input please check the format");
      &mywarn($line); }

    return $cfg;
}

sub parse_input_rate
{
    my ($cfg, $line) = @_;


    my @chunks = split('\s*;\s*', $line);

    if(scalar(@chunks) eq 5){
      my $rate_name = $chunks[0];
      $rate_name =~ s/\s*<R:(\S+)>.*/$1/;
      my $data_type = $chunks[1];
      if(not(defined($cfg->{input_rates}->{$rate_name}))){
        push @{$cfg->{input_rates_index}}, $rate_name;}
      $cfg->{input_rates}->{$rate_name}->{$data_type}->{values} = $chunks[2];
      $cfg->{input_rates}->{$rate_name}->{$data_type}->{scale}  = $chunks[3];
      $cfg->{input_rates}->{$rate_name}->{$data_type}->{units}  = $chunks[4];

      # removing commas
      $cfg->{input_rates}->{$rate_name}->{$data_type}->{values} =~ s#,# #g;
      # stripping out extra spaces
      $cfg->{input_rates}->{$rate_name}->{$data_type}->{values}=~ s#\s+# #g;
    }
    else{
      &mywarn("Unable to process the following infusion");
      &mywarn("rate please check the format");
      &mywarn($line); }

    return $cfg;
}


#
# Initializing the data structure for an indexed species
#
sub initialize_species
{

    my ($cfg, $species) = @_;

    if(not(defined($cfg->{species}->{$species}))){
      push @{$cfg->{species_index}}, $species;
      @{$cfg->{species}->{$species}->{production}}   = ();
      @{$cfg->{species}->{$species}->{consumption}}  = ();
      @{$cfg->{species}->{$species}->{odes}}         = ();
    }

    return $cfg;
}

sub initialize_parameter_set
{

    my ($cfg, $set_id) = @_;

    if(not(defined($cfg->{parameter_sets}->{$set_id}))){
      push @{$cfg->{parameter_sets_index}}, $set_id;
      $cfg->{parameter_sets}->{$set_id}->{values}  = {};
      $cfg->{parameter_sets}->{$set_id}->{name}    = undef;
    }

    return $cfg;
}

sub parse_initial_conditions
{
    my ($cfg, $line) = @_;
    # stripping off the preceding <I> and any space surrounding it
    $line      =~ s/\s*<I>\s*//;

    # if there is an equal sign, then we
    # proceed
    if($line =~ m/\S+\s*=\s*\S+/){
      my @elements = split(/\s*=\s*/, $line);
      $cfg->{initial_conditions}->{$elements[0]} = $elements[1]; }

    return $cfg;
}



 sub parse_dynamic_secondary_parameters
 {
     my ($cfg, $line) = @_;
     # stripping off the preceding <A> and any space surrounding it
     $line      =~ s/\s*<Ad>\s*//;
 
     # if there is an equal sign, then we 
     # need to add a secondary parameter;
     if($line =~ m/\S+\s*=\s*\S+/){
       my $pname        = $line;
       my $pvalue       = $line;
       $pname      =~ s/\s*=.*//;
       $pvalue     =~ s/.*\s*=\s*(.*)/$1/;
 
       # if it has not been added to the  secondary_declarations
       # hash yet, then we add it
      if(defined($cfg->{dynamic_secondary_parameters}->{$pname})){
        &mywarn("Dynamic Secondary Parameter -->$pname<-- defined multiple times, skipping subsequent entries");}
       else {
       push @{$cfg->{dynamic_secondary_parameters_index}}, $pname; 
         $cfg->{dynamic_secondary_parameters}->{$pname} = $pvalue; 
          }
     }
     else{
        &mywarn("Unable to process the following line");
        &mywarn("please check the format");
        &mywarn($line);}
     
 
 
     return $cfg;
 
 }

sub parse_static_secondary_parameters
{
    my ($cfg, $line) = @_;
    # stripping off the preceding <A> and any space surrounding it
    $line      =~ s/\s*<As>\s*//;

    # if there is an equal sign, then we 
    # need to add a secondary parameter;
    if($line =~ m/\S+\s*=\s*\S+/){
      my $pname        = $line;
      my $pvalue       = $line;
      $pname      =~ s/\s*=.*//;
      $pvalue     =~ s/.*\s*=\s*(.*)/$1/;

      # if it has not been added to the  secondary_declarations
      # hash yet, then we add it
      if(defined($cfg->{static_secondary_parameters}->{$pname})){
        &mywarn("Static Secondary Parameter -->$pname<-- defined multiple times, skipping subsequent entries");}
      else{
      push @{$cfg->{static_secondary_parameters_index}}, $pname; 
             $cfg->{static_secondary_parameters}->{$pname} = $pvalue    ; }
    }
     else{
        &mywarn("Unable to process the following line");
        &mywarn("please check the format");
        &mywarn($line);}


    return $cfg;

}

sub parse_parameter
{
    my ($cfg, $line, $ptype) = @_;

    # stripping off the preceding <P> or <VP> and any space surrounding it
    if($ptype    eq 'system'){
      $line      =~ s/\s*<P>\s*//;}
    elsif($ptype    eq 'variance'){
      $line      =~ s/\s*<VP>\s*//; }

    my @elements = split(/\s+/,$line);
    if(scalar(@elements) eq 7){
      my $pname  = $elements[0];
      my $pvalue = $elements[1];
      my $plb    = $elements[2];
      my $pub    = $elements[3];
      my $punits = $elements[4];
      my $pedit  = $elements[5];
      my $type   = $elements[6];
      
      if(defined($cfg->{parameters}->{$pname})){
        &mywarn("Parameter -->$pname<-- defined multiple times, skipping subsequent entries");}
      else{
          # creating a parameter entry
          $cfg->{parameters}->{$pname} = {
              value       => $elements[1],
              lower_bound => $elements[2],
              upper_bound => $elements[3],
              units       => $elements[4],
              editable    => $elements[5],
              ptype       => $ptype   ,     # defaulting to no, this is later overwritten
              type        => $elements[6] };
          # creating a parameter index entry
          push @{$cfg->{parameters_index}}, $pname;
          if($ptype    eq 'system'){
            push @{$cfg->{parameters_system_index}}, $pname;}
          elsif($ptype    eq 'variance'){
            push @{$cfg->{parameters_variance_index}}, $pname;}
          }

    }
    else{
        &mywarn("Unable to parse the line below");
        &mywarn("the wrong number of columns were found:");
        &mywarn($line);
    
    }


  return $cfg;
}

sub parse_parameter_set
{
  my ($cfg, $line) = @_;

  my $set_id         = $line;

  if($line =~ '<PSET:\S+:\S+>\s*\S+'){
  # entry for a parameter
    my $parameter_name  = $line;
    my $parameter_value = $line;

    $set_id         =~ s#<PSET:(\S+):\S+>\s*\S+#$1#;
    $parameter_name =~ s#<PSET:\S+:(\S+)>\s*\S+#$1#;
    $parameter_value=~ s#<PSET:\S+:\S+>\s*(\S+)#$1#;

    $cfg = &initialize_parameter_set($cfg, $set_id);
    $cfg->{parameter_sets}->{$set_id}->{values}->{$parameter_name} = $parameter_value;

  }
  elsif($line =~ '<PSET:\S+>\s*\S+'){
  # defining the name of the set
    my $set_name        = $line;
    $set_id         =~ s#<PSET:(\S+)>\s*\S+.+#$1#;
    $set_name       =~ s#<PSET:\S+>\s*(\S+.+)#$1#;

    $cfg = &initialize_parameter_set($cfg, $set_id);
    $cfg->{parameter_sets}->{$set_id}->{name}  = $set_name;
  }

  return $cfg;
}


sub parse_error_equation
{
  my ($cfg, $line) = @_;

  # line contains odes
  if($line =~ '<DV:\S+>\s*\S+'){
    my $output   = $line;
    my $equation = $line;
    # pulling out the species and ode
    $output   =~ s#\s*<DV:\s*(\S+)\s*>\s*\S+.+#$1#;
    $equation =~ s#\s*<DV:\s*\S+\s*>\s*(\S+.+)#$1#;

    $cfg->{error}->{equations}->{$output} = $equation;
  }
  else{
      &mywarn("error equation entry found but format is unexpected" ); 
      &mywarn("entry: -->$line<--"); }

  return $cfg;
}



sub parse_variance_equation
{
  my ($cfg, $line) = @_;

  # line contains odes
  if($line =~ '<VE:\S+>\s*\S+'){
    my $output   = $line;
    my $equation = $line;
    # pulling out the species and ode
    $output   =~ s#\s*<VE:\s*(\S+)\s*>\s*\S+.+#$1#;
    $equation =~ s#\s*<VE:\s*\S+\s*>\s*(\S+.+)#$1#;

    $cfg->{variance}->{equations}->{$output} = $equation;
  }
  else{
      &mywarn("variance equation entry found but format is unexpected" ); 
      &mywarn("entry: -->$line<--"); }

  return $cfg;
}

sub parse_if
{
    my ($cfg, $line) = @_;

    my $if_name  = $line;
    my $if_value = $line;
    my $if_option= $line;
    my $if_cond  = '';


    $if_name   =~ s/\s*<IF:(\S+):\S+>\s*.*/$1/g;
    $if_option =~ s/\s*<IF:\S+:(\S+)>\s*.*/$1/g;
    $if_value  =~ s/\s*<IF:\S+:\S+>\s*(.*)/$1/g;


    if(not(exists($cfg->{if_conditional}->{$if_name}))){
      # the trigger tells the script when to define
      # this if statement
      if(not(exists($cfg->{dynamic_secondary_parameters}->{$if_name})) &
         not(exists($cfg->{static_secondary_parameters}->{$if_name}))){
         &mywarn('A conditional was specified for '.$if_name);
         &mywarn('but no dynamic or static secondary parameter');
         &mywarn('has been specified. You need to initialize');
         &mywarn('initialize this parameter:');
         &mywarn('<Ad> '.$if_name.' = 0.0');
         &mywarn('or');
         &mywarn('<As> '.$if_name.' = 0.0');
        }

      $cfg->{if_conditional}->{$if_name}->{condition}      = (); # 
      $cfg->{if_conditional}->{$if_name}->{value}          = (); # 
      $cfg->{if_conditional}->{$if_name}->{else}           = ''; # 
    }


    # adding the condition for the if statement
    if('COND' eq $if_option){
      ($if_cond, $if_value) = split(/;/, $if_value);
      # stripping out any extra spaces
      $if_cond  =~ s#\s*##g;
      $if_value =~ s#\s*##g;
      push @{$cfg->{if_conditional}->{$if_name}->{condition}}, $if_cond ; # 
      push @{$cfg->{if_conditional}->{$if_name}->{value}},     $if_value; # 
    }


    # adding the statements for when 
    # the condition is false
    if('ELSE' eq $if_option){
        $cfg->{if_conditional}->{$if_name}->{else} = $if_value;
    }

    return $cfg;
}

sub extract_coeff       
{
  my ($ostr) = @_;

  my $coeffs;

  my $octr = 0;

  $coeffs->{isgood} = 1;
  $coeffs->{msg}    = "Init";




  # Getting the number of ['s in ostr
  my $nleft  = ($ostr =~ tr/\[//);
  my $nright = ($ostr =~ tr/\]//);

  # if the number of brackets don't match we give an error
  if($nleft == $nright){
    # indices of the brackets and plus
    my $ridx = 0;
    my $lidx = 0;
    my $pidx = 0;

    # offset to skip the last set of indices
    my $offset = 0;

    # current coefficient and species
    my $coeff = '';
    my $species = '';

    # now we walk through the string extracting the coeff terms
    while($octr < $nleft){

      $lidx =  index($ostr, "[", $offset);
      $ridx =  index($ostr, "]", $lidx);
      $pidx =  index($ostr, "+", $ridx);


      # First we pull out the coefficient
      $coeff = substr($ostr, $lidx+1, $ridx-$lidx-1);

      # Next we pull out the species
      # If pidx is -1 then we've reached the last term and everything is
      # relative to the length of the string. Otherwise it's relative to the
      # location of the plus
      if($pidx == -1){
        $species = substr($ostr, $ridx+1, length($ostr)-$ridx);
      } else {
        $species = substr($ostr, $ridx+1, $pidx-$ridx-1);
      }

      print "$offset [$lidx ]$ridx +$pidx C>$coeff< S>$species<\n";

      # Now we store the species/coeff
      $coeffs->{species}->{$species} = $coeff;

    
      # updating the offset and counter
      $offset = $pidx + 1;
      $octr++;
    }
  } else {
    $coeffs->{isgood} = 0;
    $coeffs->{msg}    = "The number of '[' ($nleft) does not match the number of ']' ($nright)";
  }

  # Storing everything for debugging
  $coeffs->{count}->{left}  = $nleft;
  $coeffs->{count}->{right} = $nright;
  $coeffs->{ostr} = $ostr;

  return $coeffs;
}

sub parse_equation_rate
{
    my ($cfg, $line) = @_;

    my $elements;
    my $species ;
    my $coeffs;    # coefficient information extracted from string
    my $spcoeff;   # coefficient for a specific species
    my $ostr;
    my @rels;      # array containing contributions to rate of individual species (rate elements)
    my $rstr;      # consumption or production rate string for a species

    $elements->{rate}   = $line;
    $elements->{rate}   =~ s/.*=(\S+)=>.*/$1/;
    
    # getting the left and right hand side
    ($elements->{lhs}, $elements->{rhs})  = split(/=\S+=>/, $line);

    # removing all the spaces
    $elements->{lhs} =~ s#\s+##g;
    $elements->{rhs} =~ s#\s+##g;

    # looking to see if we have any coefficients
    if($elements->{lhs} =~ m#\[# | $elements->{rhs} =~ m#\[#){
      # finding the coeff terms
      # $ ostr has both the RHS and the LHS catted together to
      # create a string we can walk through
      $ostr = $elements->{lhs}."+".$elements->{rhs};

      # now we extract the coefficients that are not 1 for the different species
      $coeffs = &extract_coeff($ostr);

      # Stripping out the order terms
      $elements->{lhs} =~ s#\[.*?\]##g;
      $elements->{rhs} =~ s#\[.*?\]##g;
    }

    # breaking up each side into the different species
    my @lhs_components =  split(/\+/,$elements->{lhs});
    my @rhs_components =  split(/\+/,$elements->{rhs});
    my @all_components   = (@lhs_components, @rhs_components);

    #
    # creating the rate from the lhs and coefficients from coeffs
    #
    foreach $species (@lhs_components){
      # by default the coefficent will be 1
      $spcoeff = '1';
      if(defined($coeffs->{species}->{$species})){
        $spcoeff = $coeffs->{species}->{$species};
      }

      # Adding the contribution of the reactants 
      # to the rate
      if($spcoeff == '1'){
        push @rels, "$species";
      } else {
        push @rels, "SIMINT_POWER[$species][$spcoeff]";
      }
    }

    # making sure the data structure is present for each species
    foreach $species (@all_components){
       $cfg = &initialize_species($cfg, $species); }


    #
    # processing the left hand side
    #
    foreach $species (@lhs_components){

      # the rate string is the forward rate times the reactant contributions
      $rstr = $elements->{rate}."*".join("\*", @rels);

      # if there is a coefficient that is not 1 we add that here
      if(defined($coeffs->{species}->{$species})){
        $rstr =  "($coeffs->{species}->{$species})*$rstr";
      }

      # adding the consumption terms 
      push @{$cfg->{species}->{$species}->{consumption}}, $rstr;
    }

    #
    # processing the right hand side
    #
    foreach $species (@rhs_components){
      # the rate string is the forward rate times the reactant contributions
      $rstr = $elements->{rate}."*".join("\*", @rels);

      # if there is a coefficient that is not 1 we add that here
      if(defined($coeffs->{species}->{$species})){
        $rstr =  "($coeffs->{species}->{$species})*$rstr";
      }
    
      # the right hand side is produced at the specified rate
      push @{$cfg->{species}->{$species}->{production}}, $rstr;
    }

    return $cfg;
}

#
# when an equation line is encountered it is deconstructed 
# and the components affecting  mass balance are added to 
# each indexed species
#
 
sub parse_equation_S
{
    my ($cfg, $line) = @_;

    my $elements;

    # removing all the spaces
    $elements->{species}   = $line;
    $elements->{species}   =~ s/.*<S:(\S+)>.*/$1/;
    
    # making sure the species has been initialized
    $cfg = &initialize_species($cfg, $elements->{species});

    ($elements->{lhs}, $elements->{rhs})  = split(/<S:\S+>/, $line);


    # removing all the spaces
    $elements->{lhs} =~ s#\s+##g;
    $elements->{rhs} =~ s#\s+##g;

    # adding production terms
    if($elements->{lhs} ne ''){
      push @{$cfg->{species}->{$elements->{species}}->{production}}, split(/;/, $elements->{lhs});
    }


    # adding consumption terms
    if($elements->{rhs} ne ''){
      push @{$cfg->{species}->{$elements->{species}}->{consumption}}, split(/;/, $elements->{rhs});
    }


    return $cfg;
}
 
sub parse_equation_C
{
    my ($cfg, $line) = @_;

    # removing all the spaces
    $line =~ s#\s+##g;

    # breaking up the line into a string with the left
    # hand side and a string with the right hand side
    my ($lhs, $rhs) =  split(/<C>/, $line);

    # now I'm chopping up each of those
    my (@lhs_components) = split(/;/, $lhs);
    my (@rhs_components) = split(/;/, $rhs);

    if((@rhs_components eq 3) and (@lhs_components eq 3)){
      # making sure the species have been initialized
      $cfg = &initialize_species($cfg, $lhs_components[0]);
      $cfg = &initialize_species($cfg, $rhs_components[0]);
      
      #adding the consumption and production terms
      push @{$cfg->{species}->{$lhs_components[0]}->{production}},    "$rhs_components[2]*$rhs_components[0]*$rhs_components[1]/$lhs_components[1]";
      push @{$cfg->{species}->{$lhs_components[0]}->{consumption}},   "$lhs_components[2]*$lhs_components[0]";
      
      push @{$cfg->{species}->{$rhs_components[0]}->{production}},    "$lhs_components[2]*$lhs_components[0]*$lhs_components[1]/$rhs_components[1]";
      push @{$cfg->{species}->{$rhs_components[0]}->{consumption}},   "$rhs_components[2]*$rhs_components[0]";
    }
    else{
       &mywarn(" There is something wrong with the following entry:");
       &mywarn(" $line");
       &mywarn(" It should have the following format:");
       &mywarn(" state; volume rate <C> state; volume; rate");
    
    }


    return $cfg;
}

sub parse_equation_fr_rate
{

    my ($cfg, $line) = @_;
    my $elements;
    my $tmp_line;

    # stripping out spaces
    $line =~ s/\s//g;

    # determining the forward (kf), and reverse (kr) rates
    $elements->{all_rates}   = $line;
    $elements->{all_rates}   =~ s/.*<=(\S+)=>.*/$1/;

    $elements->{kf} = $elements->{all_rates};
    $elements->{kr} = $elements->{all_rates};

    $elements->{kf}  =~ s#(\S+):\S+#$1#;
    $elements->{kr}  =~ s#\S+:(\S+)#$1#;

    my @equation =  split(/<=\S+=>/, $line);

    $elements->{lhs} = $equation[0];
    $elements->{rhs} = $equation[1]; 

    # Converting the reversible reaction into two reactions

    # Forward reaction:
    $tmp_line = $elements->{lhs}."=".$elements->{kf}."=>".$elements->{rhs};
    $cfg  = &parse_equation_rate($cfg, $tmp_line);

    # Reverse reaction:
    $tmp_line = $elements->{rhs}."=".$elements->{kr}."=>".$elements->{lhs};
    $cfg  = &parse_equation_rate($cfg, $tmp_line);



  return $cfg;


}

sub parse_equation_fr_rate_old
{

    my ($cfg, $line) = @_;
    my $elements;
    my $species;

    # stripping out spaces
    $line =~ s/\s//g;

    # determining the kon, koff, 
    # and KD governing this equation
    $elements->{all_rates}   = $line;
    $elements->{all_rates}   =~ s/.*<=(\S+)=>.*/$1/;

    $elements->{kon}  = $elements->{all_rates};
    $elements->{koff} = $elements->{all_rates};

    $elements->{kon}   =~ s#(\S+):\S+#$1#;
    $elements->{koff}  =~ s#\S+:(\S+)#$1#;

    my @equation =  split(/<=\S+=>/, $line);

    $elements->{lhs} = $equation[0];
    $elements->{rhs} = $equation[1]; 

    #
    # Now breaking each side up and gathering the indexed components
    #
    my @lhs_components   = split('\+', $elements->{lhs});
    my @rhs_components   = split('\+', $elements->{rhs});
    my @all_components   = (@lhs_components, @rhs_components);


    # making sure the data structure is present for each species
    foreach $species (@all_components){
       $cfg = &initialize_species($cfg, $species); }

    #
    # processing the left hand side
    #
    foreach $species (@lhs_components){
      #
      # adding the production and consumption terms 
      #
      # the right hand side produces at a rate koff
      push @{$cfg->{species}->{$species}->{production}},   $elements->{koff}."*".join("\*", @rhs_components);
      # the left hand side is consumed at a rate kon
      push @{$cfg->{species}->{$species}->{consumption}},  $elements->{kon} ."*".join("\*", @lhs_components);
    }

    #
    # processing the right hand side
    #
    foreach $species (@rhs_components){
      #
      # adding the production and consumption terms 
      #
      # the right hand side is consumed at a rate koff
      push @{$cfg->{species}->{$species}->{consumption}},   $elements->{koff}."*".join("\*", @rhs_components);
      # the left hand side is produced at a rate kon
      push @{$cfg->{species}->{$species}->{production}},  $elements->{kon} ."*".join("\*", @lhs_components);
    }


    #push @{$cfg->{reaction_elements}}, $elements; 


  return $cfg;


}


sub strip_element
{
   my ($list, $pattern) = @_;
   my @result = ();
   my $element;

   
   # comparing each element in the list to the pattern
   # and if the element is not equal to the pattern it is added
   # to the list
   foreach $element (@{$list}){
     if(not($element eq $pattern)){
       push @result, $element;
     }
   }
   return @result;
}

sub unique {
    return keys %{{ map { $_ => 1 } @_ }};
}


#
# for estimation software (e.g. Adapt) it may not be useful to have all of the
# outputs specified in the system file. So if no variance equations are
# specified, then all of the non QC_ outputs are going to be dumped to the
# target otherwise, only the outputs with corresponding variance equations
# will be dumped to the file.
#
sub fetch_estimateable_outputs {

  my ($cfg) = @_;

  my @all_outputs = @{$cfg->{outputs_index}};
  my $output;
  my @est_outputs =();
  

  # checking to see if there are any variance equations
  if( keys %{$cfg->{variance}->{equations}}){
    # if there are then these are the estimateable outptus
    @est_outputs = keys %{$cfg->{variance}->{equations}};
  }else{
    # otherwise we just return all of the non QC outputs
    foreach $output (@all_outputs){
      if($output !~ m#^QC_#){
        push  @est_outputs, $output;
      }
    }
  }
  
  return @est_outputs;
}

sub fetch_padding {
   my ($string, $max_length) = @_;

  
   my $padding = '';

   if($max_length > length($string)){
     $padding = ' 'x($max_length-length($string)); }

   return  $padding;

}

sub mywarn {
   my ($string) = @_;

   print " #-> $string\n";
}

sub extract_conditional{
# For piecewise continuous variables the different elements
# are extracted, formatting is applied to convert the 
# generic SIMINT functions into language specific functions, 
# and the conditional if/then structure is then created
# for the language-specific target output

   my ($cfg, $parameter, $output_type) = @_;
   my $return_text = "";
   my $assignment  = '';

   my $tmp_cond       = '';
   my $tmp_value      = '';
   my $tmp_cond_idx   = 0;


   
     # checking to see if the conditional follows the current parameter
     if(defined($cfg->{if_conditional}->{$parameter})){

       if($output_type eq 'pw'){
         $return_text = 'piecewise(';}
   
       while($tmp_cond_idx < scalar(@{$cfg->{if_conditional}->{$parameter}->{condition}})){
   
         $tmp_cond        = $cfg->{if_conditional}->{$parameter}->{condition}->[$tmp_cond_idx];
         $tmp_value       = $cfg->{if_conditional}->{$parameter}->{value}->[$tmp_cond_idx]; 
   
         # performing substitutions for language specific functions
         $tmp_cond  = &apply_format($cfg, $tmp_cond, $output_type);
         $tmp_value = &apply_format($cfg, $tmp_value,$output_type);
         
         #
         # defining the conditional and true statements
         #
         # C output
         if($output_type eq 'C'){
           if($tmp_cond_idx eq 0){
           $return_text .= "if($tmp_cond){\n  $parameter = $tmp_value;\n}"; }
           else{
           $return_text .= "else if($tmp_cond){\n  $parameter = $tmp_value;\n}"; }
         }
         # matlab output
         elsif($output_type eq 'matlab'){
           if($tmp_cond_idx eq 0){
             $return_text .= "if($tmp_cond) $parameter = $tmp_value; "; }
           else{
             $return_text .= "elseif($tmp_cond) $parameter = $tmp_value; "; } 
         }
         # monolix output
         elsif($output_type eq 'monolix'){
           if($tmp_cond_idx eq 0){
             $return_text .= "if($tmp_cond)\n    $parameter = $tmp_value; \n"; }
           else{
             $return_text .= "  elseif($tmp_cond)\n    $parameter = $tmp_value; \n"; } 
         }
         # potterswheel
         elsif($output_type eq 'pw'){
           $return_text .= "$tmp_value, $tmp_cond,"; }
         # perkeley madonna
         elsif($output_type eq 'bm'){
           $return_text .= " IF $tmp_cond THEN $tmp_value"; }
         elsif($output_type eq 'fortran'){
           if($tmp_cond_idx eq 0){
             $return_text .= &fortranify_line("IF ($tmp_cond) THEN ");
             $return_text .= &fortranify_line("   $parameter = $tmp_value");}
           else{
             $return_text .= &fortranify_line("ELSE IF ($tmp_cond) THEN ");
             $return_text .= &fortranify_line("   $parameter = $tmp_value ");}
         }
         elsif($output_type eq 'nonmem'){
           if($tmp_cond_idx eq 0){
             $return_text .= "IF ($tmp_cond) THEN  \n";
             $return_text .= "   $parameter = $tmp_value \n";}
           else{
             $return_text .= "ELSE IF ($tmp_cond) THEN \n";
             $return_text .= "   $parameter = $tmp_value \n";}
         }
         elsif($output_type eq 'rproject'){
           if($tmp_cond_idx eq 0){
             $return_text .= "if($tmp_cond){\n  $parameter = $tmp_value  \n"; }
           else{
             $return_text .= "}else if($tmp_cond){\n  $parameter = $tmp_value  \n"; } 
         }
   
   
         $tmp_cond_idx = $tmp_cond_idx + 1;
       }
   
       #
       # defining the else condition
       #
       $tmp_value = $cfg->{if_conditional}->{$parameter}->{else};
       $tmp_value = &apply_format($cfg, $tmp_value,$output_type);
   
       if($output_type eq 'C'){
         $return_text .= "else{\n  $parameter = $tmp_value;} \n"; }
       elsif($output_type eq 'matlab'){
         $return_text .= "else $parameter = $tmp_value;  end"; }
       elsif($output_type eq 'monolix'){
         $return_text .= "  else \n    $parameter = $tmp_value;\nend\n"; }
       elsif($output_type eq 'pw'){
         $return_text .= "$tmp_value)"; }
       elsif($output_type eq 'bm'){
         $return_text .= " ELSE $tmp_value"; }
       elsif($output_type eq 'fortran'){
         $return_text .= &fortranify_line("ELSE") ;
         $return_text .= &fortranify_line("   $parameter = $tmp_value"); 
         $return_text .= &fortranify_line("END IF") ;
        }
       elsif($output_type eq 'nonmem' ){
         $return_text .= "ELSE \n" ;
         $return_text .= "   $parameter = $tmp_value \n"; 
         $return_text .= "END IF \n" ;
        }
       elsif($output_type eq 'rproject'){
         $return_text .= "}else{ \n  $parameter = $tmp_value \n}\n"; }
     }

   return $return_text;

}

sub data_check{
  my ($cfg) = @_;

  # this checks to see if a data file has been specified.
  if($cfg->{data}->{file} ne ''){
    # if there is a datafile we check to see if it exists
    if (-e $cfg->{data}->{file} ) {
      # now we read in the data file
      my @lines;
      my $file_handle;
      my @cols;
      open(EQFH, $cfg->{data}->{file} ) or die 'Unable to open data file';
      
      # Reading in each line in system file
      { local $/=undef;  $file_handle=<EQFH>; }
      @lines=split /[\r\n]+/, $file_handle;
      close(EQFH);
 
      # Now @lines has all of the rows of the file. If automatic header
      # determination has been selected we're going to attempt to read those
      if($cfg->{data}->{headers}->{mode} eq ''){
        &mywarn("A data file has been specified by no header information was provided");}
      elsif($cfg->{data}->{headers}->{mode} eq 'automatic'){
        @{$cfg->{data}->{headers}->{values}} = split(',', $lines[0]);
      
      }
      elsif($cfg->{data}->{headers}->{mode} eq 'manual'){
        # checking the number of manual values specified and comparing that to
        # the number in the data file
        @cols = split(',', $lines[0]);

        if(scalar(@cols) ne scalar( @{$cfg->{data}->{headers}->{values}})){
          &mywarn("Data header label mismatch");
          &mywarn("Data file contains:      ".scalar( @cols));
          &mywarn("Manual columns specifed: ".scalar( @{$cfg->{data}->{headers}->{values}}));
          }
       }
    }
    else{ 
     &mywarn("The specified data file does not exist $cfg->{data}->{file}");}
  }
}

sub system_check{
  my ($cfg) = @_;

  my $name       = '';
  my $set_name   = '';
  my $cond_name  = '';
  my @nmdatacols = ();
  my $tmpstring  = '';

   
  # checking to make sure at least one
  # output has been defined
   
  if(not((keys %{$cfg->{outputs}}) >0)){
    &mywarn('No Outputs were specified. This can cause');
    &mywarn('problems in the GUI and elsewhere. It is ');
    &mywarn('suggested that you create a dummy output:');
    &mywarn('<O> Temp = 1.0');
  
  }

  foreach $name (keys %{$cfg->{variance}->{equations}}){
    if(not(defined($cfg->{outputs}->{$name}))){
      &mywarn("Error model specified for output: $name.");
      &mywarn("However, there is no output by that name.");
    }
  
  }

  foreach $name (keys %{$cfg->{error}->{equations}}){
    if(not(defined($cfg->{outputs}->{$name}))){
      &mywarn("Error equation specified for output: $name.");
      &mywarn("However, there is no output by that name.");
    }
  
  }


  # checking all of the components for name conflicts
  foreach $name (keys %{$cfg->{outputs}}){
    &namespace_check($cfg, $name, 'outputs');
  }

  foreach $name (keys %{$cfg->{parameters}}){
    &namespace_check($cfg, $name, 'parameters');
  }

  if(keys(%{$cfg->{iiv_index}})){
    foreach $set_name (keys(%{$cfg->{iiv_index}})){
      foreach $name (keys %{$cfg->{iiv}->{$set_name}->{iivs}}){
        &namespace_check($cfg, $name, 'iiv');
      }
    }
  }

  foreach $name (keys %{$cfg->{species}}){
    &namespace_check($cfg, $name, 'species');
  }

  foreach $name (keys %{$cfg->{static_secondary_parameters}}){
    &namespace_check($cfg, $name, 'static_secondary_parameters');
  }


  foreach $name (keys %{$cfg->{dynamic_secondary_parameters}}){
    &namespace_check($cfg, $name, 'dynamic_secondary_parameters');
  }

  foreach $name (keys %{$cfg->{input_rates}}){
    &namespace_check($cfg, $name, 'input_rates');
  }


  if(keys(%{$cfg->{iiv_index}})){
    foreach $set_name (keys(%{$cfg->{iiv_index}})){
      foreach $name (keys %{$cfg->{iiv}->{$set_name}->{iivs}}){
        # making sure the variance has been specified for each IIV term
        if(not(defined($cfg->{iiv}->{$set_name}->{vcv}->{$name}->{$name}))){
          &mywarn("The IIV $name was defined but no covariance");
          &mywarn('value was specified:');
          &mywarn("<IIV:$name> 0.1");
        }
      }
    }
  }

  if(keys(%{$cfg->{iiv_index}})){
    foreach $set_name (keys(%{$cfg->{iiv_index}})){
      foreach $name (keys %{$cfg->{iiv}->{$set_name}->{parameters}}){
        # making sure the variance has been specified for each IIV term
        if(not(defined($cfg->{parameters}->{$name}))){
          &mywarn("The IIV was applied to the system parameter $name,");
          &mywarn("but this parameter has not been defined");
          &mywarn("<P> bob value lb ub units yes grouping ");
        }
      }
    }
  }
   
  # Checking conditional statements for 'else' condition:
  foreach $cond_name (keys(%{$cfg->{if_conditional}})){
    if($cfg->{if_conditional}->{$cond_name}->{else} eq ''){
      &mywarn('The secondary parameter '.$cond_name.'');
      &mywarn('has a specified conditional statemnet.');
      &mywarn('However it lacks a default "else" condition.');
      &mywarn('This can be specified in the following way:');
      &mywarn('<IF:'.$cond_name.':ELSE> 0.0');
    }
 }

  
  # checking parameter sets to make sure that the parameters specified in the
  # set have been generally defined for the system

  foreach $set_name (keys(%{$cfg->{parameter_sets}})){
    foreach $name (keys(%{$cfg->{parameter_sets}->{$set_name}->{values}})){  
      if(not(defined($cfg->{parameters}->{$name}))){
        &mywarn("The parameter '$name' in the set '$set_name' was defined.");
        &mywarn("However, '$name' was not defined as a general system parameter.");
      }
    }
  }


  # If both a data set and covariates are specified we check to see if the
  # columns in the dataset have those covariates specified

  if((@{$cfg->{covariates_index}})
    and ($cfg->{data}->{file} ne "")){
  
    # first we get a list of all the columns that are being used in NONMEM. We
    # iterate through all of the columns in the data file
    foreach $name  (@{$cfg->{data}->{headers}->{values}}){
    #Checking first to see if the column has been renamed
    if(defined($cfg->{options}->{nonmem}->{input}->{rename}->{$name})){
      # we put the renamed column on the list of data columns
      push @nmdatacols, $cfg->{options}->{nonmem}->{input}->{rename}->{$name};
    }
    else{
      #otherwise we just add the column name
      push  @nmdatacols, $name; }
    }

    my $tmpstring = ":::".join(':::', @nmdatacols).":::";

    # next we loop through each covariate and see if it's in the list of
    # nonmem data columns
    foreach $name  (@{$cfg->{covariates_index}}){
      if(not($tmpstring =~ m#:::${name}:::#)){
       &mywarn("The covariate: $name was specified but not defined in dataset"); }
    }
    }



}

#----------------------------------------------------------------------------------


sub namespace_check{
  my ($cfg, $name, $self) = @_;

  my $conflict = '';

  #my $element;
  my $isgood = '';

  my $set_name;

  my $program;
  my $word   ;


  # going through the reserved words
  foreach $program (keys(%{$cfg->{reserved}})){
    foreach $word   (keys(%{$cfg->{reserved}->{$program}})){
      $isgood = 'yes';
      if($cfg->{reserved}->{$program}->{$word}  eq 'exact'){
        if($name eq $word)   {
          $isgood = 'no';
        } 
      }
      if($cfg->{reserved}->{$program}->{$word}  eq 'start'){
        if($name =~ m#^$word#){
          $isgood = 'no';
        } 
      }
      
      if($cfg->{reserved}->{$program}->{$word}  eq 'insensitive'){
        if($name =~ m#^$word$#i){
          $isgood = 'no';
        } 
      }
      
      if($isgood eq 'no'){
      $conflict = $name;
      &mywarn("Conflict: The name ($name) conflicts with $word used in $program "); }
    }
  }

  # checking indices for:
  #    parameters
  #    static_secondary_parameters
  #    dynamic_secondary_parameters
  #    species
  #    outputs 


  if(exists($cfg->{parameters}->{$name}) and ($self ne 'parameters')){
    $conflict = $name;
    &mywarn("Conflict: The $self name ($name) conflicts with a parameter name"); }

  if(exists($cfg->{static_secondary_parameters}->{$name}) and ($self ne 'static_secondary_parameters')){
    $conflict = $name;
    &mywarn("Conflict: The $self name ($name) conflicts with a static_secondary_parameter name"); }

  if(exists($cfg->{dynamic_secondary_parameters}->{$name}) and ($self ne 'dynamic_secondary_parameters')){
    $conflict = $name;
    &mywarn("Conflict: The $self name ($name) conflicts with a dynamic_secondary_parameter name"); }

  if(exists($cfg->{outputs}->{$name}) and ($self ne 'outputs') ){
    $conflict = $name;
    &mywarn("Conflict: The $self name ($name) conflicts with an output name"); }

  if(exists($cfg->{species}->{$name})  and ($self ne 'species')){
    $conflict = $name;
    &mywarn("Conflict: The $self name ($name) conflicts with a species name"); }

  if(exists($cfg->{input_rates}->{$name}) and ($self ne 'input_rates')){
    $conflict = 'yes';
    &mywarn("Conflict: The $self name ($name) conflicts with an input_rates"); }

  if(keys(%{$cfg->{iiv_index}})){
    foreach $set_name (keys(%{$cfg->{iiv_index}})){
      if(exists($cfg->{iiv}->{$set_name}->{iivs}->{$name}) and ($self ne 'iiv')){
        $conflict = 'yes';
        &mywarn("Conflict: The $self name ($name) conflicts with an iiv in $set_name"); }
    }
  }

  return $conflict;

}

sub make_iiv{
   my ($cfg, $name, $format, $set_id) = @_;
   # this function returns the equation used to calculate add variability to a
   # parameter $name based on the language 'format'

   my $distribution = $cfg->{iiv}->{$set_id}->{parameters}->{$name}->{distribution};
   my $calc_string  = '';


   if($distribution eq 'LN'){
        $calc_string = 'SIMINT_PARAMETER_TV*SIMINT_EXP[SIMINT_IIV_VALUE]'; }
   elsif($distribution eq 'N'){
        $calc_string = 'SIMINT_PARAMETER_TV*(1.0 + SIMINT_IIV_VALUE)'; }
   elsif($distribution eq 'WB'){
        my $OPK = @{$cfg->{iiv}->{$set_id}->{parameters}->{$name}->{other_parameters}}[0];
        my $OPL = @{$cfg->{iiv}->{$set_id}->{parameters}->{$name}->{other_parameters}}[1];
        $calc_string = 'TODO';} #"($OPK/$OPL)*SIMINT_POWER[SIMINT_IIV_VALUE/$OPL][$OPK-1]*SIMINT_EXP[-SIMINT_POWER[SIMINT_IIV/$OPL][$OPK]]";}
   elsif($distribution eq 'BC'){
        my $OPL1 = @{$cfg->{iiv}->{$set_id}->{parameters}->{$name}->{other_parameters}}[0];
        my $OPL2 = @{$cfg->{iiv}->{$set_id}->{parameters}->{$name}->{other_parameters}}[1];
        $calc_string = 'TODO';}
   else{
    &mywarn("Distribution $distribution for IIV on $name not found"); }



   # applying language specific format
   $calc_string = &apply_format($cfg, $calc_string, $format);

   return  $calc_string;
}

sub make_ode{
 my ($cfg, $species_name, $format) = @_;
 # this function returns a string with the full ODE for species_name 
 # with the format (e.g., 'fortran') applied 

  my $tmp_ode   = "";
  if((@{$cfg->{species}->{$species_name}->{odes}})){
    if($tmp_ode eq ""){
      $tmp_ode .= join('+', @{$cfg->{species}->{$species_name}->{odes}}); }
    else{
      $tmp_ode .= "+".join('+', @{$cfg->{species}->{$species_name}->{odes}}); }
   }
  
  if((@{$cfg->{species}->{$species_name}->{production}})){
   if($tmp_ode eq ""){
     $tmp_ode .= join('+', @{$cfg->{species}->{$species_name}->{production}}); }
   else{
     $tmp_ode .= "+".join('+', @{$cfg->{species}->{$species_name}->{production}}); }
  }
  
  if((@{$cfg->{species}->{$species_name}->{consumption}})){
     $tmp_ode .= "-(".join('+', @{$cfg->{species}->{$species_name}->{consumption}}).")"; }
  
  # converting generic functions into fortran functions
  $tmp_ode   = &apply_format($cfg, $tmp_ode, $format);

  return $tmp_ode;

 }


sub apply_function{
 my ($test_string, $start_pattern, $num_arguments, $new_pattern) = @_;

 #  $test_string     = 'SIMINT_POWER[SIMINT_EXP[SIMINT_POWER[c][c]]][w]';
 #  $start_pattern   = 'SIMINT_POWER[';
 #  $num_arguments   = 2;
 #  $new_pattern     = 'pow(SIMINT_ARG_0, SIMINT_ARG_1)';
 #

 my $result = &find_bracketed_arguments($test_string, $start_pattern, $num_arguments);

 my $argument_idx;

 # if we haven't encountered any errors above, we'll go through and substitute the
 # function with the new pattern and replaced arguments
 if($result->{error} < 1){
   #replacing the arguments in new_pattern
   for($argument_idx=0;$argument_idx<scalar(@{$result->{arguments}}); $argument_idx++){
     $new_pattern =~ s#SIMINT_ARG_$argument_idx#$result->{arguments}->[$argument_idx]#g;
   }
   #replacing the old pattern with the new one
   substr($test_string, $result->{function_start}, ($result->{argument_end}-$result->{function_start}), $new_pattern);
 }


 return $test_string;

}


#----------------------------#
# Start:                     #
# Model Target Templates     #
#----------------------------#


#
# Reads in template file with placeholders to be populated 
#
sub fetch_template_file{

  my ($cfg, $file_name) = @_;
  my $template = '';

  
  # fetching the relative path
  # $file_name = &catfile('library', 'templates', $file_name);
  $file_name = &catfile( $cfg->{files}->{templates}, $file_name);


  # reading the contents into template
  local $/ = undef;
  open FILE, $file_name or die "Couldn't open file: $!";
  binmode FILE;
  $template = <FILE>;
  close FILE;

 return $template;
}


sub fetch_matlab_auto_analysis_estimation
{
my $template ='clear; close all;

% flowctl - Controls the flow of the analysis script
%{
flowctl = \'estimate\';
flowctl = \'plot previous estimate\';
flowctl = \'previous estimate as guess\';
%}
flowctl = \'plot guess\';
analysis_name = \'ANAME\';

% Compiling the system to make sure the 
% latest changes have been committed. 
build_system

% Setting up paths
provision_workspace;

% Defining color codes to be used 
% in plotting below
mc = fetch_color_codes;

% Pulling out the system information
cfg = auto_fetch_system_information;

<PSETS>
% Select the parameter set and the parameters to estimate:
%{
% The following will estimate a subset of the parameters:
pnames ={ \'PNAME1\'
          \'PNAME2\' 
          \'PNAME3\' }; 
cfg  = system_select_set(cfg, \'default\', pnames);

% This will estimate all of the parameters:
cfg  = system_select_set(cfg, \'default\');
%}


%{
% To overwrite the initial guess at the scripting level for the parameter
% PNAME with the value VALUE use the following:
cfg = system_set_guess(cfg, \'PNAME\', VALUE);
%}

% Specifying Simulation and Estimation options
%
% help system_set_option
%{

% Simulation options:
% If you have simulink you should use it:
cfg = system_set_option(cfg, \'simulation\', \'integrate_with\', \'simulink\');

% Set the output times for smooth profiles when plotting. This will be the
% default output times unless overwritten at the cohort level:
cfg = system_set_option(cfg, \'simulation\', ...
                             \'output_times\', ...
                              <OUTPUT_TIMES>);
% Solver options
cfg = system_set_option(cfg, \'solver\',     \'RelTol\',  1e-5);
cfg = system_set_option(cfg, \'solver\',     \'AbsTol\',  1e-9);
cfg = system_set_option(cfg, \'solver\',     \'solver\', \'ode23s\');

% Estimation options:
% % Monitoring the estimation process:
cfg = system_set_option(cfg, \'estimation\', \'monitor_iteration_history\', 100);
cfg = system_set_option(cfg, \'estimation\', \'monitor_slope_tolerance\',   0.001);
cfg = system_set_option(cfg, \'estimation\', \'monitor_exit_when_stable\', \'yes\');

% To disable the monitoring set the status function to \'\'
% cfg = system_set_option(cfg, \'estimation\', \'monitor_status_function\', \'\');
%
% % This disables the monitoring and performs a sort of simulated annealing:
cfg = system_set_option(cfg, \'estimation\', \'effort\', 100);
%
% % Overwrite optimization options:
% To use the Nelder-Mead method:
cfg = system_set_option(cfg, \'estimation\', \'optimizer\', \'fminsearch\');
cfg = system_set_option(cfg, \'estimation\', \'optimization_options\', ...
              optimset(\'Display\',   \'iter\', ...
                       \'TolFun\',     1e-3,  ...
                       \'MaxIter\',    3000,  ...
                       \'MaxFunEval\', 3000));

% If you have the Global Optimization Toolbox:
% To use the Genetic Algorithm
cfg = system_set_option(cfg, \'estimation\', \'optimizer\', \'ga\');
cfg = system_set_option(cfg, \'estimation\', \'optimization_options\', ...
             gaoptimset(\'Display\',      \'iter\', ...
                        \'MaxGenerations\', 500)); 

%}

% Loading datasets   
%
% help system_load_dataset
%{
% From Excel sheet
cfg = system_load_dataset(cfg, \'DSNAME\'     , ...
                         sprintf(\'data%sDSFILE.xls\', filesep), ...
                         \'SHEETNAME\');
% From csv        
cfg = system_load_dataset(cfg, \'DSNAME\'     , ...
                         sprintf(\'data%sDSFILE.csv\', filesep))
% From tab        
cfg = system_load_dataset(cfg, \'DSNAME\'     , ...
                         sprintf(\'data%sDSFILE.tab\', filesep))
%}


% Defining the cohorts
%
% Clearing all of the cohorts
cfg = system_clear_cohorts(cfg);
 
% One entry for each cohort:
% For more information type:
%
% help system_define_cohort
%
% It is necessary to replace the following compontents:
%
% CHNAME    - cohort name
% COLNAME   - column name in dataset
% ONAME     - output name
% TIMECOL   - column name in dataset with the observation times
% TS        - model timescale corresponding to TIMECOL
% OBSCOL    - column name in dataset with the observation values
% MODOUTPUT - model output corresponding to OBSCOL
%
% Only specify inputs that are non-zero. Simply ignore inputs that do not
% exist for the given cohort.
%{
clear cohort;
cohort.name                                 = \'CHNAME\';
cohort.cf.COLNAME                           = [];
cohort.cf.COLNAME                           = [];
cohort.dataset                              = \'DSNAME\';
<BOLUS><INFUSION_RATES><COVARIATES> 
cohort.outputs.ONAME.of.COLNAME             = [];
cohort.outputs.ONAME.of.COLNAME             = [];
cohort.outputs.ONAME.obs.time               = \'TIMECOL\'; 
cohort.outputs.ONAME.obs.value              = \'OBSCOL\';
cohort.outputs.ONAME.model.time             = \'TS\';        
cohort.outputs.ONAME.model.value            = \'MODOUTPUT\';  
cohort.outputs.ONAME.model.variance         = \'1\';  
% Optional
cohort.outputs.ONAME.options.marker_color   = \'r\';
cohort.outputs.ONAME.options.marker_shape   = \'o\';
cohort.outputs.ONAME.options.marker_line    = \'-\'; 

% Adding the cohort:
cfg = system_define_cohort(cfg, cohort);
%}

% Getting the parameters either by estimation, loading the previous estimation
% results, or pulling out the initial guess
%
% The variable pest will be defined here and used below for running
% simulations:
pnames_est = cfg.estimation.parameters.names;
if(strcmp(flowctl , \'estimate\') | strcmp(flowctl, \'previous estimate as guess\'))

  % Checking the analysis name
  name_check = ubiquity_name_check(analysis_name)   ;
  if(~name_check.isgood)
    vp(cfg, sprintf(\'Error: the analyssis name >%s< is invalid\', analysis_name  ))
    vp(cfg, sprintf(\'Problems: %s\', name_check.msg))
    analysis_name   = \'analysis\';
    vp(cfg, sprintf(\'Instead Using: %s\', analysis_name  ))
  end

  % Loading the previous estimate
  if(strcmp(flowctl, \'previous estimate as guess\'))
    eval(sprintf(\'load output%sanalysis_%s.mat pest ss pnames_est\',filesep, analysis_name))
    cfg.estimation.parameters.guess = pest;
  end

  % Performing the parameter estimation: 
  [pest, ss] = estimate_parameters(cfg);

  % Saving the results:
  eval(sprintf(\'save output%sanalysis_%s.mat pest ss pnames_est\',filesep, analysis_name))
  archive_estimation(analysis_name, cfg);
elseif(strcmp(flowctl , \'plot guess\'))
  pest   = system_fetch_guess(cfg);
else 
  % Loading the previously saved results:
  eval(sprintf(\'load output%sanalysis_%s.mat pest ss pnames_est\',filesep, analysis_name))
end


% Cohorts defined here will be plotted 
% but not used during the estimation

% Running simulation for plotting
%
[erp] = system_simulate_estimation_results(pest, cfg);

% Plotting the observations and predictions
%
plot_opts = [];
%{
% save figures to file with analysis_name prefix
plot_opts.save_figs = analysis_name;

% For each output the following can be defined
% to control plotting.  See help system_plot_cohorts 
% for more options and details.
plot_opts.outputs.ONAME.panels   = \'no\';
plot_opts.outputs.ONAME.rows     = 1;
plot_opts.outputs.ONAME.cols     = 2;
plot_opts.outputs.ONAME.yscale   = \'log\';
plot_opts.outputs.ONAME.ylim     = [1,    100];
plot_opts.outputs.ONAME.xlim     = [0,    100];
plot_opts.outputs.ONAME.ylabel   = \'Output (units)\';
plot_opts.outputs.ONAME.xlabel   = \'Time (units)\';
plot_opts.outputs.ONAME.datatype = \'all\';
%}



close all;
system_plot_cohorts(erp, plot_opts, cfg);

';

return $template;

}

sub fetch_pw_template3
{

my $pwtemplate = '
% PottersWheel model definition file
% This file is constructed as a Matlab function.
% The returned variable "m" contains all required information of the model.
% For more information visit www.potterswheel.de

function m = target_pw()

m             = pwGetEmptyModel();

% General information:
% ID fields require no blanks and only A-Za-z_0-9. 
% Use [] in order to get default values for a field.
% You can use an arbitrary number of strings in arrays like {\'string1\',\'string2\', ...}.

%% Meta information

m.name        = \'target_pw3\';
m.description = \'Autogenerated PottersWheel Model Target\';
m.authors     = {};
m.dates       = {\'\'};
m.modelFormat = 3.0;


%% X: Dynamic variables
% m = pwAddX(m, ID, startValue, type, minValue, maxValue, unit, compartment, name, description, typeOfStartValue)
% ID: Unique name of the player, e.g. \'ProtA\'
% startValue: the initial concentration (default 0)
% type: \'global\', \'local\' (default), \'fix\' (startValue will be fitted globally or locally or is fixed)
% minValue and maxValue specify the minimum and maximum of the startValue during fitting
% unit: not yet used
% compartment: compartment of the player. Default: first given compartment
% typeOfStartValue: \'amount\' or \'concentration\' (default)
% Not listed players which occur in the reactions get the default settings

<STATES>

%% ODE
% m = pwAddODE(m, leftHandSide, rightHandSide)

<ODES>

%% C: Compartments
% m = pwAddC(m, ID, size,  outside, spatialDimensions, name, unit, constant)

m = pwAddC(m,     \'Central\');

%% K: Dynamical parameters
% m = pwAddK(m, ID, value, type, minValue, maxValue, unit, name, description)
% type: \'global\', \'local\', \'fix\' (during fitting)
% value: value of the parameter
% minValue and maxValue specify the extreme values for fitting
% E.g. m = pwAddK(m, \'Stat_act\', 1.2, \'global\', 0, 100);

<PARAMETERS>

%% A: 

% m = pwAddA(m, lhs, rhs, [], [], type)

<SECONDARY_PARAMETERS>

%% U - Driving inputs
% m = pwAddU(m, *ID, *uType, *uTimes, *uValues, compartment, name, description, u2Values, alternativeIDs, designerProps, classname, referenceXID, unit, uFormula)
% Some entities like the ligand concentration can be controlled externally.
% Here you can specify the default dependency on time of these players.
% When loading an experiment, the default dependency is usually overwritten.
% Example:
% A step input starting at t=-100 at level 0, jumping at t=0 to 5 and decreasing
% at t=10 to level 2:
% m = pwAddU(m, \'L\', \'steps\', [-100 0 10], [0 5 2], \'cell\');


<INPUTS>
%% Y - Observables
% m = pwAddY(m, *ID, *rhs, errorModelRhs, noiseType, unit, name, description, alternativeIDs, designerProps, classname)
% ID:          ID of the observable
% rhs:         right-hand side of the observation, i.e. a function of all dynamic variables X, dynamic parameters K, and observation parameters S
% errorModel:  formula to calculate the standard deviation depending on measurements y
% noiseType:   \'Gaussian\' (other noise types will be implemented later)
% Example with Gaussian noise with a standard deviation of 10 % relative to y plus 5 % absolute (relative to max(y) over all y):
% m = pwAddY(m, \'totalStat_obs\', \'scale_totalStat_obs * (Stat + pStat + 2 * pStatDimer)\', \'0.10 * y + 0.05 * max(y)\', \'Gaussian\');

<OUTPUTS>

';

 
 
return $pwtemplate;

}

sub fetch_comments
{

  my ($comments_raw, $format) = @_;

  my $comments = $comments_raw;

  if($format eq 'matlab'){
    # getting rid of the single quotes
    $comments =~ s#'#`#g;
    # putting in the matlab comment string '#'
    $comments =~ s/SIMINT_COMMENT_STRING/#/g;
    # converting the comments into a cell array
    $comments =~ s#\n#'}\n{'#g;
    $comments = "[{'".$comments."'}]";
  }
  elsif($format eq 'bm'){
    $comments =~ s/SIMINT_COMMENT_STRING/;/g;
  }
  elsif($format eq 'rproject'){
    $comments =~ s/SIMINT_COMMENT_STRING/#/g;
  }
  elsif($format eq 'monolix'){
    $comments =~ s/SIMINT_COMMENT_STRING//g;
  }


  return $comments;

}

sub fetch_pw_template2
{

my $pwtemplate = '
% PottersWheel model definition file
% This file is constructed as a Matlab function.
% The returned variable "m" contains all required information of the model.
% For more information visit www.potterswheel.de

function m = target_pw()

m             = pwGetEmptyModel();

% General information:
% ID fields require no blanks and only A-Za-z_0-9. 
% Use [] in order to get default values for a field.
% You can use an arbitrary number of strings in arrays like {\'string1\',\'string2\', ...}.

%% Meta information

m.ID          = \'target_pw\';
m.name        = \'Autogenerated PottersWheel Model Target\';
m.description = \'\';
m.authors     = {};
m.dates       = {\'\'};


%% X: Dynamic variables
% m = pwAddX(m, ID, startValue, type, minValue, maxValue, unit, compartment, name, description, typeOfStartValue)
% ID: Unique name of the player, e.g. \'ProtA\'
% startValue: the initial concentration (default 0)
% type: \'global\', \'local\' (default), \'fix\' (startValue will be fitted globally or locally or is fixed)
% minValue and maxValue specify the minimum and maximum of the startValue during fitting
% unit: not yet used
% compartment: compartment of the player. Default: first given compartment
% typeOfStartValue: \'amount\' or \'concentration\' (default)
% Not listed players which occur in the reactions get the default settings

<STATES>

%% ODE
% m = pwAddODE(m, leftHandSide, rightHandSide)

<ODES>

%% C: Compartments
% m = pwAddC(m, ID, size,  outside, spatialDimensions, name, unit, constant)

m = pwAddC(m,     \'Central\');

%% K: Dynamical parameters
% m = pwAddK(m, ID, value, type, minValue, maxValue, unit, name, description)
% type: \'global\', \'local\', \'fix\' (during fitting)
% value: value of the parameter
% minValue and maxValue specify the extreme values for fitting
% E.g. m = pwAddK(m, \'Stat_act\', 1.2, \'global\', 0, 100);

<PARAMETERS>

%% A: 

% m = pwAddA(m, lhs, rhs, [], [], type)

<SECONDARY_PARAMETERS>

%% U: Driving input
% m = pwAddU(m, ID, uType, uTimes, uValues, compartment, name, description, u2Values, alternativeIDs, designerProps)

<INPUTS>

%% Y: Observables
% m = pwAddY(m, rhs, ID, scalingParameter, errorModel, noiseType, unit, name, description, alternativeIDs, designerProps)
% rhs: right hand side of the observation, i.e. a function of all variables
% ID:  ID of the observable
% errorModel: formula to calculate the standard deviation depending on measurements y
% m = pwAddY(m, \'Stat + pStat + 2 * pStatDimer\', \'totalStat_obs\', \'scale_totalStat_obs\', \'0.10 * y + 0.05 * max(y)\', \'Gaussian\');

<OUTPUTS>

';

 
 
return $pwtemplate;

}

#----------------------------#
# Stop:                     #
# Model Target Templates     #
#----------------------------#
