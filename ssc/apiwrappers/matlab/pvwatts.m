% SSC SDK - MATLAB example
%
% example script to run PVWatts

clear

SSC.ssccall('load');

% create a data container to store all the variables
data = SSC.ssccall('data_create');

% setup the system parameters
SSC.ssccall('data_set_string', data, 'file_name', '../../examples/abilene.tm2');
SSC.ssccall('data_set_number', data, 'system_size', 4);
SSC.ssccall('data_set_number', data, 'derate', 0.77);
SSC.ssccall('data_set_number', data, 'track_mode', 0);
SSC.ssccall('data_set_number', data, 'tilt', 30);
SSC.ssccall('data_set_number', data, 'azimuth', 180);

% create the PVWatts module
module = SSC.ssccall('module_create', 'pvwattsv1');

% run the module
ok = SSC.ssccall('module_exec', module, data);
if ok,
    % if successful, retrieve the hourly AC generation data and print
    % annual kWh on the screen
    ac = SSC.ssccall('data_get_array', data, 'ac');
    disp(sprintf('pvwatts: %.2f kWh',sum(ac)/1000.0));
else
    % if it failed, print all the errors
    disp('pvwattsv1 errors:');
    ii=0;
    while 1,
        err = SSC.ssccall('module_log', module, ii);
        if strcmp(err,''),
            break;
        end
        disp( err );
        ii=ii+1;
    end
end

% free the PVWatts module that we created
SSC.ssccall('module_free', module);

% release the data container and all of its variables
SSC.ssccall('data_free', data);

% unload the library
SSC.ssccall('unload');