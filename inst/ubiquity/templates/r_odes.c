#include <R.h>

static double parms[<NPARAMS>];<FORCEDECLARE>

<SYSTEM_PARAM>

<FORCE_PARAM>

/* initializing the parameters and forcing functions  */
void initparams(void (* odeparms)(int *, double *))
{
    int N=<NPARAMS>;
    odeparms(&N, parms);
}

<FORCEFUNC>

/* Derivatives and outputs  */
void derivs (int *neq, 
             double *t, 
             double *y, 
             double *ydot,
             double *yout, 
             int *ip)
{

if (ip[0] <1) error("nout should be at least 1");

/* Start Initializing variables 
*/
double SIMINT_TIME = 0.0;

<VARIABLE_INIT>
/* Done Initializing variables */

/* System time and time scales*/
SIMINT_TIME = *t; 
<TIME_SCALES>

<SS_PARAM>

/* Mapping states to named variables */
<STATES>

<DS_PARAM>

/* Defining the ODEs*/
<ODES>

/* Mapping back to ydot variables */
<ODES_REMAP>

/* Defining the outputs*/
<OUTPUTS>

/* Mapping back to yout variables */
<OUTPUTS_REMAP>
}
