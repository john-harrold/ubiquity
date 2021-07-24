


function system_<MODEL_PREFIX>_fetch_cfg!()
#
# This function returns a dictionary that stores all of the information about the
# system including parameter values, system indices used, initial condition
# assignments, etc.
#

# Creating the cfg variable


c_libfile_base = "<MODEL_PREFIX>"


# storing the location of the temporary directory and the distribution type
cfg = Dict()
cfg["options"] = Dict("misc"=>Dict("temp_directory" => "<TEMP_DIRECTORY>",
                                   "distribution"   => "<DISTRIBUTION>",
                                   "system_file"    => "<SYSTEM_FILE>"))


end



function system_<MODEL_PREFIX>_DYDT!(SIMINT_dx, SIMINT_x, SIMINT_p, SIMINT_TIME, SIMINT_cfg)

# System parameters
<SYSTEM_PARAM>


end

