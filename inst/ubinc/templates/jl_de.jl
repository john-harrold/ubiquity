#
# How do we include time-varying inputs? 
#


# p[0]   to p[p-1]     represents the system parameters    (p=> number of parameters)
# p[p]   to p[p+r-1]   represents the rate inputs          (r=> number of infusion rate inputs) 
# p[p+r] to p[p+r+v-1] represents the timevarying inputs   (v=> number of time varying inputs) 


function system_DYDT!(SIMINT_du, SIMINT_u, SIMINT_p, SIMINT_TIME)
<SYSTEM_PARAM><SS_PARAM><STATES><DS_PARAM>

# Defining the differential equations
<ODES>

# Mapping back to du variables
<ODES_REMAP>

return(SIMINT_du)
end 
