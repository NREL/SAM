function [result] = ssccall(action, arg0, arg1, arg2 )
% SAM Simulation Core (SSC) MATLAB API
% Copyright (c) 2012 National Renewable Energy Laboratory
% author: Aron P. Dobos and Steven H. Janzou

    % automatically detect architecture to load proper dll.
    [pathstr, fn, fext] = fileparts(mfilename('fullpath'));
    if ( strcmp(computer(), 'PCWIN') ) % Windows 32-bit
        ssclibpath = '../../../win32/';
        ssclib = 'ssc';
    elseif ( strcmp(computer(), 'PCWIN64') ) % Windows 64-bit
        ssclibpath = '../../../win64/';
        ssclib = 'ssc';
    elseif ( strcmp(computer(), 'MACI64') ) % Mac Intel 64-bit
        ssclibpath = '../../../osx64/';
        ssclib = 'ssc';
    elseif ( strcmp(computer(), 'GLNXA64') ) % Linux 64-bit
        ssclibpath = '../../../linux64/';
        ssclib = 'ssc';
    end

    % load proper ssc library for all functions
    if ~libisloaded(ssclib)
        oldFolder = cd(pathstr);
        loadlibrary(strcat(ssclibpath,ssclib),strcat(ssclibpath,'../sscapi.h'));
        cd(oldFolder);
    end
    
    if strcmp(action,'load')
        if ~libisloaded(ssclib)
            oldFolder = cd(pathstr);
            loadlibrary(strcat(ssclibpath,ssclib),strcat(ssclibpath,'../sscapi.h'));
            cd(oldFolder);
        end

    elseif strcmp(action,'unload')
        if libisloaded(ssclib)
            unloadlibrary(ssclib)    
        end

    elseif strcmp(action,'version')
        result = calllib(ssclib,'ssc_version');

    elseif strcmp(action,'build_info')
        result = calllib(ssclib, 'ssc_build_info');

    elseif strcmp(action,'data_create')
        result = calllib(ssclib, 'ssc_data_create');
        if ( isnullpointer(result) )
            result = 0;
        end

    elseif strcmp(action,'data_free')
        result = calllib(ssclib, 'ssc_data_free', arg0);

    elseif strcmp(action,'data_unassign')
        result = calllib(ssclib, 'ssc_data_unassign', arg0, arg1);

    elseif strcmp(action,'data_query')
        result = calllib(ssclib, 'ssc_data_query', arg0, arg1 );

    elseif strcmp(action,'data_first')
        result = calllib(ssclib, 'ssc_data_first', arg0 );

    elseif strcmp(action,'data_next')
        result = calllib(ssclib, 'ssc_data_next', arg0 );

    elseif strcmp(action,'data_set_string')
        result = calllib(ssclib, 'ssc_data_set_string', arg0, arg1, arg2 );

    elseif strcmp(action,'data_set_number')
        result = calllib(ssclib, 'ssc_data_set_number', arg0, arg1, single(arg2) );

    elseif strcmp(action,'data_set_array')
        len = length(arg2);
        arr = libpointer( 'singlePtr', arg2 );
        result = calllib(ssclib,'ssc_data_set_array',arg0,arg1,arr,len);

    elseif strcmp(action,'data_set_matrix')
        [nr nc] = size(arg2);
        mat = zeros(nr*nc, 1);
        ii = 1;
        for r=1:nr,
            for c=1:nc,
                mat(ii) = arg2(r,c);
                ii=ii+1;
            end
        end
        arr = libpointer( 'singlePtr', mat );
        result = calllib(ssclib,'ssc_data_set_matrix',arg0,arg1,arr,nr,nc);

    elseif strcmp(action,'data_set_table')
        result = calllib(ssclib,'ssc_data_set_table',arg0,arg1,arg2);

    elseif strcmp(action,'data_get_string')
        result = calllib(ssclib,'ssc_data_get_string',arg0,arg1);

    elseif strcmp(action,'data_get_number')
         p = libpointer('singlePtr',0);
         calllib(ssclib,'ssc_data_get_number', arg0,arg1,p);
         result = get(p,'Value');

    elseif strcmp(action,'data_get_array')
        p_count = libpointer('int32Ptr',0);   
        [xobj] = calllib(ssclib,'ssc_data_get_array',arg0,arg1,p_count);
        setdatatype(xobj,'int32Ptr',p_count.Value,1);
        len = p_count.Value;
        result = zeros( len, 1 );
        for i=1:len,
            pidx = xobj+(i-1);
            setdatatype(pidx,'singlePtr',1,1);
            result(i) = pidx.Value;
        end

    elseif strcmp(action,'data_get_matrix')
        p_rows = libpointer('int32Ptr',0);
        p_cols = libpointer('int32Ptr',0);
        [xobj] = calllib(ssclib,'ssc_data_get_matrix',arg0,arg1,p_rows,p_cols);
        setdatatype(xobj,'int32Ptr',p_rows.Value*p_cols.Value,1);
        nrows = p_rows.Value;
        ncols = p_cols.Value;
        if ( nrows*ncols > 0 )
            result = zeros( nrows, ncols );
            ii=1;
            for r=1:nrows,
                for c=1:ncols,
                    pidx = xobj+(ii-1);
                    setdatatype(pidx,'singlePtr',1,1);
                    result(r,c) = pidx.Value;
                    ii=ii+1;
                end
            end
        end

    elseif strcmp(action,'data_get_table')
        result = calllib(ssclib,'ssc_data_get_table',arg0,arg1);

    elseif strcmp(action,'module_entry')
        result = calllib(ssclib,'ssc_module_entry',arg0);
        if isnullpointer( result ),
            result = 0;
        end

    elseif strcmp(action,'entry_name')
        result = calllib(ssclib,'ssc_entry_name',arg0);

    elseif strcmp(action,'entry_description')
        result = calllib(ssclib,'ssc_entry_description',arg0);

    elseif strcmp(action,'entry_version')
        result = calllib(ssclib,'ssc_entry_version',arg0);
        
    elseif strcmp(action,'module_var_info')
        result = calllib(ssclib,'ssc_module_var_info',arg0,arg1);
        if isnullpointer( result ),
            result = 0;
        end
        
    elseif strcmp(action,'info_var_type')
        ty = calllib(ssclib,'ssc_info_var_type',arg0);
        if (ty == 1)
            result = 'input';
        elseif ( ty==2 )
            result = 'output';
        else
            result = 'inout';
        end
        
    elseif strcmp(action,'info_data_type')
        dt = calllib(ssclib,'ssc_info_data_type',arg0);
        if (dt == 1)
            result = 'string';
        elseif (dt == 2)
            result = 'number';
        elseif (dt == 3)
            result = 'array';
        elseif (dt == 4)
            result = 'matrix';
        elseif (dt == 5)
            result = 'table';
        else
            result = 'invalid';
        end
        
    elseif strcmp(action,'info_name')
        result = calllib(ssclib,'ssc_info_name',arg0);
        
    elseif strcmp(action,'info_label')
        result = calllib(ssclib,'ssc_info_label',arg0);
        
    elseif strcmp(action,'info_units')
        result = calllib(ssclib,'ssc_info_units',arg0);
        
    elseif strcmp(action,'info_meta')
        result = calllib(ssclib,'ssc_info_meta',arg0);
        
    elseif strcmp(action,'info_group')
        result = calllib(ssclib,'ssc_info_group',arg0);
        
    elseif strcmp(action,'info_required')
        result = calllib(ssclib,'ssc_info_required',arg0);
        
    elseif strcmp(action,'info_constraints')
        result = calllib(ssclib,'ssc_info_constraints',arg0);
        
    elseif strcmp(action,'info_uihint')
        result = calllib(ssclib,'ssc_info_uihint',arg0);
        
    elseif strcmp(action,'exec_simple')
        result = calllib(ssclib,'ssc_module_exec_simple',arg0,arg1);
        
    elseif strcmp(action,'exec_simple_nothread')
        result = calllib(ssclib,'ssc_module_exec_simple_nothread',arg0,arg1);

    elseif strcmp(action,'module_create')
        result = calllib(ssclib,'ssc_module_create',arg0);
        if ( isnullpointer(result) )
            result = 0;
        end

    elseif strcmp(action,'module_free')
        result = calllib(ssclib,'ssc_module_free',arg0);

    elseif strcmp(action,'module_exec')
        result = calllib(ssclib,'ssc_module_exec',arg0,arg1);

    elseif strcmp(action,'module_log')
        p_type = libpointer('int32Ptr',1);
        p_time = libpointer('singlePtr',1);
        result = calllib(ssclib,'ssc_module_log', arg0, arg1, p_type, p_time);
        
    elseif strcmp(action,'module_log_detailed')
        p_type = libpointer('int32Ptr',1);
        p_time = libpointer('singlePtr',1);
        text = calllib(ssclib,'ssc_module_log', arg0, arg1, p_type, p_time);
        typetext = 'notice';
        if (p_type.Value == 2)
            typetext = 'warning';
        elseif (p_type.Value == 3)
            typetext = 'error';
        end
        if ( strcmp(text,'') )
            result = 0;
        else
            result = {text , typetext , p_time.Value};
        end
        
    else
        disp( sprintf('ssccall: invalid action %s', action) );        
        result = 0;
    end

end

function bb = isnullpointer(p)
    bb = false;
    try
        setdatatype(p, 'voidPtr', 1, 1);
        deref = get(p);
    catch
        e = lasterror();
        if strcmp(e.identifier, 'MATLAB:libpointer:ValueNotDefined')
            bb = true;
        end
    end
end

