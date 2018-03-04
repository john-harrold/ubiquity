$PROBLEM problem placeholder
; Currently the NONMEM target is being developed
; and it is not considered usable.

$INPUT
<INPUT>

$DATA <DATA>

$SUB ADVAN13 TOL=4 

$MODEL 

; Defining the compartment names
<COMP_ASSIGNMENT>


$PK
; Defining the system parameters
<PARAMETERS_MAP> 
<IIV_MAP> 
<IIV_ON_PARAMETERS>   

; Defining the static 
; secondary parmaeters
<STATIC_SECONDARY_PARAMETERS> <INFUSION_RATES>

; Initial conditions
<INITIAL_CONDITIONS>


$DES 

; creating the default internal time variable
SIMINT_TIME = TIME
; Mapping the amounts to meaningful names
<STATES_ASSIGNMENT>

; Defining secondary paraemters that can
; change with time
<DYNAMIC_SECONDARY_PARAMETERS>

; Differential equations for each compartment
<ODES_ASSIGNMENT>

; Mapping thes named ODEs above back to the 
; appropriate DADT variables
<ODES_MAP>

$ERROR
; The SIEB (Simulation Internal Error Block) prefix
; is added to variables that are used in both
; the DES and ERROR blocks

; creating the default internal time variable
SIEB_TIME = TIME

; Mapping the states to their names
<STATES_ASSIGNMENT_NMEB>

; Defining the dynamic
; secondary parameters
<DYNAMIC_SECONDARY_PARAMETERS_NMEB>

;mapping variance parameters to named values
<VARIANCE_ASSIGNMENT>

$THETA
<PARAMETER_VALUES>


<IIV_VALUES>

$SIGMA
<VARIANCE_PARAMETER_VALUES>

