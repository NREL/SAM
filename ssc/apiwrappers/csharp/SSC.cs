// SAM Simulation Core (SSC) C# API
// Copyright (c) 2012 National Renewable Energy Laboratory
// author: Steven H. Janzou and Aron P. Dobos

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;

namespace SSC
{

    class sscapi
    {
        static sscapi()
        {
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_version")]
        public static extern int ssc_version32();
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_version")]
        public static extern int ssc_version64();
        public static int ssc_version()
        {
            return (System.IntPtr.Size == 8) ? ssc_version64() : ssc_version32();
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_build_info")]
        public static extern IntPtr ssc_build_info32();
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_build_info")]
        public static extern IntPtr ssc_build_info64();
        public static IntPtr ssc_build_info()
        {
            return (System.IntPtr.Size == 8) ? ssc_build_info64() : ssc_build_info32();
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_create")]
        public static extern IntPtr ssc_data_create32();
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_create")]
        public static extern IntPtr ssc_data_create64();
        public static IntPtr ssc_data_create()
        {
            return (System.IntPtr.Size == 8) ? ssc_data_create64() : ssc_data_create32();
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_free")]
        public static extern void ssc_data_free32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_free")]
        public static extern void ssc_data_free64(HandleRef cxtData);
        public static void ssc_data_free(HandleRef cxtData)
        {
            if (System.IntPtr.Size == 8) 
            {
                ssc_data_free64(cxtData);
            }
            else
            {
                ssc_data_free32(cxtData);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_clear")]
        public static extern void ssc_data_clear32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_clear")]
        public static extern void ssc_data_clear64(HandleRef cxtData);
        public static void ssc_data_clear(HandleRef cxtData)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_clear64(cxtData);
            }
            else
            {
                ssc_data_clear32(cxtData);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_unassign")]
        public static extern void ssc_data_unassign32(HandleRef cxtData, string variableName);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_unassign")]
        public static extern void ssc_data_unassign64(HandleRef cxtData, string variableName);
        public static void ssc_data_unassign(HandleRef cxtData, string variableName)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_unassign64(cxtData, variableName);
            }
            else
            {
                ssc_data_unassign32(cxtData, variableName);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_query")]
        public static extern int ssc_data_query32(HandleRef cxtData, string variableName);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_query")]
        public static extern int ssc_data_query64(HandleRef cxtData, string variableName);
        public static int ssc_data_query(HandleRef cxtData, string variableName)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_query64(cxtData, variableName) : ssc_data_query32(cxtData, variableName);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_first")]
        public static extern IntPtr ssc_data_first32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_first")]
        public static extern IntPtr ssc_data_first64(HandleRef cxtData);
        public static IntPtr ssc_data_first(HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_first64(cxtData) : ssc_data_first32(cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_next")]
        public static extern IntPtr ssc_data_next32(HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_next")]
        public static extern IntPtr ssc_data_next64(HandleRef cxtData);
        public static IntPtr ssc_data_next(HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_next64(cxtData) : ssc_data_next32(cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_string")]
        public static extern void ssc_data_set_string32(HandleRef cxtData, string name, string value);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_string")]
        public static extern void ssc_data_set_string64(HandleRef cxtData, string name, string value);
        public static void ssc_data_set_string(HandleRef cxtData, string name, string value)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_string64(cxtData, name, value);
            }
            else
            {
                ssc_data_set_string32(cxtData, name, value);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_number")]
        public static extern void ssc_data_set_number32(HandleRef cxtData, string name, float value);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_number")]
        public static extern void ssc_data_set_number64(HandleRef cxtData, string name, float value);
        public static void ssc_data_set_number(HandleRef cxtData, string name, float value)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_number64(cxtData, name, value);
            }
            else
            {
                ssc_data_set_number32(cxtData, name, value);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_array")]
        public static extern void ssc_data_set_array32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_array")]
        public static extern void ssc_data_set_array64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length);
        public static void ssc_data_set_array(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[] array, int length)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_array64(cxtData, name, array, length);
            }
            else
            {
                ssc_data_set_array32(cxtData, name, array, length);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_matrix")]
        public static extern void ssc_data_set_matrix32(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_matrix")]
        public static extern void ssc_data_set_matrix64(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols);
        public static void ssc_data_set_matrix(HandleRef cxtData, string name, [In, MarshalAs(UnmanagedType.LPArray)]float[,] matrix, int nRows, int nCols)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_matrix64(cxtData, name, matrix, nRows, nCols);
            }
            else
            {
                ssc_data_set_matrix32(cxtData, name, matrix, nRows, nCols);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_table")]
        public static extern void ssc_data_set_table32(HandleRef cxtData, string name, HandleRef cxtTable);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_set_table")]
        public static extern void ssc_data_set_table64(HandleRef cxtData, string name, HandleRef cxtTable);
        public static void ssc_data_set_table(HandleRef cxtData, string name, HandleRef cxtTable)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_data_set_table64(cxtData, name, cxtTable);
            }
            else
            {
                ssc_data_set_table32(cxtData, name, cxtTable);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_string")]
        public static extern IntPtr ssc_data_get_string32(HandleRef cxtData, string name);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_string")]
        public static extern IntPtr ssc_data_get_string64(HandleRef cxtData, string name);
        public static IntPtr ssc_data_get_string(HandleRef cxtData, string name)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_string64(cxtData, name) : ssc_data_get_string32(cxtData, name);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_number")]
        public static extern int ssc_data_get_number32(HandleRef cxtData, string name, out float number);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_number")]
        public static extern int ssc_data_get_number64(HandleRef cxtData, string name, out float number);
        public static int ssc_data_get_number(HandleRef cxtData, string name, out float number)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_number64(cxtData, name, out number) : ssc_data_get_number32(cxtData, name, out number);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_array")]
        public static extern IntPtr ssc_data_get_array32(HandleRef cxtData, string name, out int len);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_array")]
        public static extern IntPtr ssc_data_get_array64(HandleRef cxtData, string name, out int len);
        public static IntPtr ssc_data_get_array(HandleRef cxtData, string name, out int len)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_array64(cxtData, name, out len) : ssc_data_get_array32(cxtData, name, out len);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_matrix")]
        public static extern IntPtr ssc_data_get_matrix32(HandleRef cxtData, string name, out int nRows, out int nCols);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_matrix")]
        public static extern IntPtr ssc_data_get_matrix64(HandleRef cxtData, string name, out int nRows, out int nCols);
        public static IntPtr ssc_data_get_matrix(HandleRef cxtData, string name, out int nRows, out int nCols)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_matrix64(cxtData, name, out nRows, out nCols) : ssc_data_get_matrix32(cxtData, name, out nRows, out nCols);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_table")]
        public static extern IntPtr ssc_data_get_table32(HandleRef cxtData, string name);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_data_get_table")]
        public static extern IntPtr ssc_data_get_table64(HandleRef cxtData, string name);
        public static IntPtr ssc_data_get_table(HandleRef cxtData, string name)
        {
            return (System.IntPtr.Size == 8) ? ssc_data_get_table64(cxtData, name) : ssc_data_get_table32(cxtData, name);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_entry")]
        public static extern IntPtr ssc_module_entry32(int moduleIndex);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_entry")]
        public static extern IntPtr ssc_module_entry64(int moduleIndex);
        public static IntPtr ssc_module_entry(int moduleIndex)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_entry64(moduleIndex) : ssc_module_entry32(moduleIndex);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_name")]
        public static extern IntPtr ssc_entry_name32(HandleRef cxtEntry);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_name")]
        public static extern IntPtr ssc_entry_name64(HandleRef cxtEntry);
        public static IntPtr ssc_entry_name(HandleRef cxtEntry)
        {
            return (System.IntPtr.Size == 8) ? ssc_entry_name64(cxtEntry) : ssc_entry_name32(cxtEntry);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_description")]
        public static extern IntPtr ssc_entry_description32(HandleRef cxtEntry);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_description")]
        public static extern IntPtr ssc_entry_description64(HandleRef cxtEntry);
        public static IntPtr ssc_entry_description(HandleRef cxtEntry)
        {
            return (System.IntPtr.Size == 8) ? ssc_entry_description64(cxtEntry) : ssc_entry_description32(cxtEntry);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_version")]
        public static extern int ssc_entry_version32(HandleRef cxtEntry);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_entry_version")]
        public static extern int ssc_entry_version64(HandleRef cxtEntry);
        public static int ssc_entry_version(HandleRef cxtEntry)
        {
            return (System.IntPtr.Size == 8) ? ssc_entry_version64(cxtEntry) : ssc_entry_version32(cxtEntry);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_create")]
        public static extern IntPtr ssc_module_create32(string moduleName);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_create")]
        public static extern IntPtr ssc_module_create64(string moduleName);
        public static IntPtr ssc_module_create(string moduleName)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_create64(moduleName) : ssc_module_create32(moduleName);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_free")]
        public static extern void ssc_module_free32(HandleRef cxtModule);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_free")]
        public static extern void ssc_module_free64(HandleRef cxtModule);
        public static void ssc_module_free(HandleRef cxtModule)
        {
            if (System.IntPtr.Size == 8)
            {
                ssc_module_free64(cxtModule);
            }
            else
            {
                ssc_module_free32(cxtModule);
            }
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_var_info")]
        public static extern IntPtr ssc_module_var_info32(HandleRef cxtModule, int index);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_var_info")]
        public static extern IntPtr ssc_module_var_info64(HandleRef cxtModule, int index);
        public static IntPtr ssc_module_var_info(HandleRef cxtModule, int index)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_var_info64(cxtModule, index) : ssc_module_var_info32(cxtModule, index);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_var_type")]
        public static extern int ssc_info_var_type32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_var_type")]
        public static extern int ssc_info_var_type64(HandleRef cxtInfo);
        public static int ssc_info_var_type(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_var_type64(cxtInfo) : ssc_info_var_type32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_data_type")]
        public static extern int ssc_info_data_type32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_data_type")]
        public static extern int ssc_info_data_type64(HandleRef cxtInfo);
        public static int ssc_info_data_type(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_data_type64(cxtInfo) : ssc_info_data_type32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_name")]
        public static extern IntPtr ssc_info_name32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_name")]
        public static extern IntPtr ssc_info_name64(HandleRef cxtInfo);
        public static IntPtr ssc_info_name(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_name64(cxtInfo) : ssc_info_name32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_label")]
        public static extern IntPtr ssc_info_label32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_label")]
        public static extern IntPtr ssc_info_label64(HandleRef cxtInfo);
        public static IntPtr ssc_info_label(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_label64(cxtInfo) : ssc_info_label32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_units")]
        public static extern IntPtr ssc_info_units32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_units")]
        public static extern IntPtr ssc_info_units64(HandleRef cxtInfo);
        public static IntPtr ssc_info_units(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_units64(cxtInfo) : ssc_info_units32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_meta")]
        public static extern IntPtr ssc_info_meta32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_meta")]
        public static extern IntPtr ssc_info_meta64(HandleRef cxtInfo);
        public static IntPtr ssc_info_meta(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_meta64(cxtInfo) : ssc_info_meta32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_group")]
        public static extern IntPtr ssc_info_group32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_group")]
        public static extern IntPtr ssc_info_group64(HandleRef cxtInfo);
        public static IntPtr ssc_info_group(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_group64(cxtInfo) : ssc_info_group32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_required")]
        public static extern IntPtr ssc_info_required32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_required")]
        public static extern IntPtr ssc_info_required64(HandleRef cxtInfo);
        public static IntPtr ssc_info_required(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_required64(cxtInfo) : ssc_info_required32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_constraints")]
        public static extern IntPtr ssc_info_constraints32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_constraints")]
        public static extern IntPtr ssc_info_constraints64(HandleRef cxtInfo);
        public static IntPtr ssc_info_constraints(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_constraints64(cxtInfo) : ssc_info_constraints32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_uihint")]
        public static extern IntPtr ssc_info_uihint32(HandleRef cxtInfo);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_info_uihint")]
        public static extern IntPtr ssc_info_uihint64(HandleRef cxtInfo);
        public static IntPtr ssc_info_uihint(HandleRef cxtInfo)
        {
            return (System.IntPtr.Size == 8) ? ssc_info_uihint64(cxtInfo) : ssc_info_units32(cxtInfo);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple")]
        public static extern int ssc_module_exec_simple32(string moduleName, HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple")]
        public static extern int ssc_module_exec_simple64(string moduleName, HandleRef cxtData);
        public static int ssc_module_exec_simple(string moduleName, HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple64(moduleName, cxtData) : ssc_module_exec_simple32(moduleName, cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple_nothread")]
        public static extern IntPtr ssc_module_exec_simple_nothread32(string moduleName, HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_simple_nothread")]
        public static extern IntPtr ssc_module_exec_simple_nothread64(string moduleName, HandleRef cxtData);
        public static IntPtr ssc_module_exec_simple_nothread(string moduleName, HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec_simple_nothread64(moduleName, cxtData) : ssc_module_exec_simple_nothread32(moduleName, cxtData);
        }
        
        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec")]
        public static extern int ssc_module_exec32(HandleRef cxtModule, HandleRef cxtData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec")]
        public static extern int ssc_module_exec64(HandleRef cxtModule, HandleRef cxtData);
        public static int ssc_module_exec(HandleRef cxtModule, HandleRef cxtData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec64(cxtModule, cxtData) : ssc_module_exec32(cxtModule, cxtData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_with_handler")]
        public static extern int ssc_module_exec_with_handler32(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_exec_with_handler")]
        public static extern int ssc_module_exec_with_handler64(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData);
        public static int ssc_module_exec_with_handler(HandleRef cxtModule, HandleRef cxtData, HandleRef cxtHandler, HandleRef cxtUserData)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_exec_with_handler64(cxtModule, cxtData, cxtHandler, cxtUserData) : ssc_module_exec_with_handler32(cxtModule, cxtData, cxtHandler, cxtUserData);
        }

        [DllImport("ssc32.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_log")]
        public static extern IntPtr ssc_module_log32(HandleRef cxtModule, int index, out int messageType, out float time);
        [DllImport("ssc64.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "ssc_module_log")]
        public static extern IntPtr ssc_module_log64(HandleRef cxtModule, int index, out int messageType, out float time);
        public static IntPtr ssc_module_log(HandleRef cxtModule, int index, out int messageType, out float time)
        {
            return (System.IntPtr.Size == 8) ? ssc_module_log64(cxtModule, index, out messageType, out time) : ssc_module_log32(cxtModule, index, out messageType, out time);
        }
    }


    public class Data
    {
        private HandleRef m_data;
        private bool m_owned;


        public Data()
        {
            m_data = new HandleRef(this, sscapi.ssc_data_create());
            m_owned = true;
        }

        public Data( IntPtr dataRefNotOwned )
        {
            m_data = new HandleRef(this, dataRefNotOwned);
            m_owned = false;
        }

        ~Data()
        {
            if (m_owned && m_data.Handle != IntPtr.Zero)
                sscapi.ssc_data_free(m_data);
        }

        public void Clear()
        {
            sscapi.ssc_data_clear(m_data);
        }

        public String First()
        {
            IntPtr p = sscapi.ssc_data_first(m_data);
            if (p != IntPtr.Zero)
                return Marshal.PtrToStringAnsi(p);
            else
                return null;
        }

        public String Next()
        {
            IntPtr p = sscapi.ssc_data_next(m_data);
            if (p != IntPtr.Zero)
                return Marshal.PtrToStringAnsi(p);
            else
                return null;
        }

        public int Query(String name)
        {
            return sscapi.ssc_data_query(m_data, name);
        }

        public void SetNumber(String name, float value)
        {
            sscapi.ssc_data_set_number(m_data, name, value);
        }

        public float GetNumber(String name)
        {
            float val = float.NaN;
            sscapi.ssc_data_get_number(m_data, name, out val);
            return val;
        }

        public void SetString(String name, String value)
        {
            sscapi.ssc_data_set_string(m_data, name, value);
        }

        public String GetString(String name)
        {
            IntPtr p = sscapi.ssc_data_get_string(m_data, name);
            return Marshal.PtrToStringAnsi(p);
        }

        public void SetArray(String name, float[] data)
        {
            sscapi.ssc_data_set_array(m_data, name, data, data.Length);
        }

        public float[] GetArray(String name)
        {
            int len;
            IntPtr res = sscapi.ssc_data_get_array(m_data, name, out len);
            float[] arr = null;
            if (len > 0)
            {
                arr = new float[len];
                Marshal.Copy(res, arr, 0, len);
            }
            return arr;
        }

        public void SetMatrix(String name, float[,] mat)
        {
            int nRows = mat.GetLength(0);
            int nCols = mat.GetLength(1);
            sscapi.ssc_data_set_matrix(m_data, name, mat, nRows, nCols);
        }

        public float[,] GetMatrix(String name)
        {
            int nRows, nCols;
            IntPtr res = sscapi.ssc_data_get_matrix(m_data, name, out nRows, out nCols);
            if (nRows * nCols > 0)
            {
                float[] sscMat = new float[nRows * nCols];
                Marshal.Copy(res, sscMat, 0, nRows * nCols);
                float[,] mat = new float[nRows, nCols];
                for (int i = 0; i < nRows; i++)
                {
                    for (int j = 0; j < nCols; j++)
                    {
                        mat[i, j] = sscMat[i * nCols + j];
                    }
                }
                return mat;
            }
            else
                return null;
        }

        public void SetTable(String name, Data table)
        {
            sscapi.ssc_data_set_table(m_data, name, table.GetDataHandle());
        }

        public Data GetTable(String name)
        {
            IntPtr p = sscapi.ssc_data_get_table(m_data, name);
            if (IntPtr.Zero == p)
                return null;
            else
                return new Data( p );
        }

        public HandleRef GetDataHandle()
        {
            return m_data;
        }
    }


    public class Module
    {
        private HandleRef m_mod;

        public Module(String name)
        {
            m_mod = new HandleRef(this, sscapi.ssc_module_create(name) );
        }

        ~Module()
        {
            if (m_mod.Handle != IntPtr.Zero)
                sscapi.ssc_module_free(m_mod);
        }

        public bool IsOk()
        {
            return m_mod.Handle != IntPtr.Zero;
        }

        public HandleRef GetModuleHandle()
        {
            return m_mod;
        }

        public bool Exec( Data data )
        {
            return (sscapi.ssc_module_exec(m_mod, data.GetDataHandle()) != 0);
        }

        public bool Log(int idx, out String msg, out int type, out float time)
        {
            msg = "";
            IntPtr p = sscapi.ssc_module_log(m_mod, idx, out type, out time);
            if (IntPtr.Zero != p)
            {
                msg = Marshal.PtrToStringAnsi(p);
                return true;
            }
            else
                return false;
        }
    }


    public class Entry
    {
        private HandleRef m_entry;
        private int m_idx;

        public Entry()
        {
            m_idx = 0;
        }

        public void Reset()
        {
            m_idx = 0;
        }

        public bool Get()
        {
            IntPtr p = sscapi.ssc_module_entry(m_idx);
            if (p == IntPtr.Zero)
            {
                Reset();
                return false;
            }

            m_entry = new HandleRef(this, p);
            m_idx++;
            return true;
        }

        public String Name()
        {
            if (m_entry.Handle != IntPtr.Zero)
            {
                IntPtr p = sscapi.ssc_entry_name(m_entry);
                return Marshal.PtrToStringAnsi(p);
            }
            else return null;
        }

        public String Description()
        {
            if (m_entry.Handle != IntPtr.Zero)
            {
                IntPtr p = sscapi.ssc_entry_description(m_entry);
                return Marshal.PtrToStringAnsi(p);
            }
            else
                return null;
        }

        public int Version()
        {
            if (m_entry.Handle != IntPtr.Zero)
                return sscapi.ssc_entry_version(m_entry);
            else
                return -1;
        }
    }

    public class Info
    {
        private HandleRef m_inf;
        private Module m_mod;
        private int m_idx;

        public Info(Module m)
        {
            m_mod = m;
            m_idx = 0;
        }

        public void Reset()
        {
            m_idx = 0;
        }

        public bool Get()
        {
            IntPtr p = sscapi.ssc_module_var_info(m_mod.GetModuleHandle(), m_idx);
            if (p == IntPtr.Zero)
            {
                Reset();
                return false;
            }

            m_inf = new HandleRef(this, p);
            m_idx++;
            return true;
        }

        public String Name()
        {
            if (m_inf.Handle == IntPtr.Zero) return null;
            IntPtr p = sscapi.ssc_info_name(m_inf);
            return Marshal.PtrToStringAnsi(p);
        }

        public int VarType()
        {
            if (m_inf.Handle == IntPtr.Zero) return -1;
            return sscapi.ssc_info_var_type(m_inf);
        }

        public int DataType()
        {
            if (m_inf.Handle == IntPtr.Zero) return -1;
            return sscapi.ssc_info_data_type(m_inf);
        }

        public string Label()
        {
            if (m_inf.Handle == IntPtr.Zero) return null;
            IntPtr p = sscapi.ssc_info_label(m_inf);
            return Marshal.PtrToStringAnsi(p);
        }

        public string Units()
        {
            if (m_inf.Handle == IntPtr.Zero) return null;
            IntPtr p = sscapi.ssc_info_units(m_inf);
            return Marshal.PtrToStringAnsi(p);
        }

        public string Meta()
        {
            if (m_inf.Handle == IntPtr.Zero) return null;
            IntPtr p = sscapi.ssc_info_meta(m_inf);
            return Marshal.PtrToStringAnsi(p);
        }

        public string Group()
        {
            if (m_inf.Handle == IntPtr.Zero) return null;
            IntPtr p = sscapi.ssc_info_group(m_inf);
            return Marshal.PtrToStringAnsi(p);
        }

        public string Required()
        {
            if (m_inf.Handle == IntPtr.Zero) return null;
            IntPtr p = sscapi.ssc_info_required(m_inf);
            return Marshal.PtrToStringAnsi(p);
        }

        public string Constraints()
        {
            if (m_inf.Handle == IntPtr.Zero) return null;
            IntPtr p = sscapi.ssc_info_constraints(m_inf);
            return Marshal.PtrToStringAnsi(p);
        }
    }

    public class API
    {
        // constants for return value of Info.VarType() (see sscapi.h)
        public const int INPUT = 1;
        public const int OUTPUT = 2;
        public const int INOUT = 3;


        // constants for out integer type in Module.Log() method (see sscapi.h)
        public const int NOTICE = 1;
        public const int WARNING = 2;
        public const int ERROR = 3;


        // constants for return value of Data.Query() and Info.DataType() (see sscapi.h)
        public const int INVALID = 0;
        public const int STRING = 1;
        public const int NUMBER = 2;
        public const int ARRAY = 3;
        public const int MATRIX = 4;
        public const int TABLE = 5;

        static public int Version()
        {
            return sscapi.ssc_version();
        }

        static public String BuildInfo()
        {
            IntPtr buildInfo = sscapi.ssc_build_info();
            return Marshal.PtrToStringAnsi(buildInfo);
        }
    }
}
