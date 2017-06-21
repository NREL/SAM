% SSC SDK - MATLAB example
%
% example script to list all modules in SSC
% for each module, generate a text dump
% of all the input/output variables, names, data types, units, etc.

clear
SSC.ssccall('load');

i=0;
while true,
    % get a pointer to information about the ith module
    entry = SSC.ssccall('module_entry', i);
    if ( entry==0 ),
        break;
    end
    
    % if the ith entry was valid, get information about the module
    name = SSC.ssccall('entry_name', entry);
    desc = SSC.ssccall('entry_description', entry);
    ver = SSC.ssccall('entry_version', entry);
    
    disp( sprintf('module: %s ver.%d [%s]', name, ver, desc) );
    
    % create an instance of the module
    % and iterate over all of the variables
    mm = SSC.ssccall('module_create', name);    
    k=0;
    while true,
        info = SSC.ssccall('module_var_info', mm, k);
        if (info == 0),
            break;
        end
        
        var_type = SSC.ssccall('info_var_type', info);
        var_data = SSC.ssccall('info_data_type', info);
        var_name = SSC.ssccall('info_name',info);
        var_label = SSC.ssccall('info_label',info);
        var_units = SSC.ssccall('info_units',info);
        var_meta = SSC.ssccall('info_meta',info);
        var_group = SSC.ssccall('info_group',info);
        var_required = SSC.ssccall('info_required',info);
        var_constraints = SSC.ssccall('info_constraints',info);
        
        disp(sprintf('  %s: "%s" [%s] %s (%s) ', var_type, var_name, var_data, var_label, var_units ));
        
        k=k+1;
    end
    
    % release the module that we created above
    SSC.ssccall('module_free', mm);
    
    i=i+1;
end

% get SSC version and build information
ver = SSC.ssccall('version');
bi = SSC.ssccall('build_info');

disp( sprintf('[%d modules found in SSC Version %d %s]', i, ver, bi ) );

% unload the library
SSC.ssccall('unload');