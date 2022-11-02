import csv
import sys

annual_energy = "ANNUAL_OUTPUT"
lcoe = "LCOE_NOM"
lppa = "LPPA_NOM"

def csv_to_dict(csvfile):
    values = {}
    for row in csvfile:
        data = {}
        try:
            key = str(row[0]) + " " + str(row[1])
            if key != "TECHNOLOGY FINANCING" and key != "final_configuration_eof none": # Skip first and last rows
                data[annual_energy] = row[2]
                data[lcoe] = row[3]
                data[lppa] = row[4]
                values[key] = data
        except IndexError as e:
            print("Error while parsing CSV: " + str(row))
            raise e
    return values

if __name__ == "__main__":

    expected_filename = sys.argv[1]
    actual_filename = sys.argv[2]
    fail_on_output = sys.argv[3] == "true"

    with open(expected_filename, "r") as datafile:
        expected_csv = csv.reader(datafile, delimiter=',', quotechar='\"')
        expected_values = csv_to_dict(expected_csv)
        
    with open(actual_filename, "r") as datafile:
        actual_csv = csv.reader(datafile, delimiter=',', quotechar='\"')
        actual_values = csv_to_dict(actual_csv)
        
    success = True
    error_base = 0.001
    error_ETES = 0.001
    error_PTES = 0.002
    
    for key in expected_values:

        error = error_base
        if(key == 'ETES Single Owner'):
            error = error_ETES
        if(key == 'PTES Single Owner'):
            error = error_PTES

        try:
            ex_data = expected_values[key]
            act_data = actual_values[key]
            if act_data:
                for k in ex_data:
                    if act_data[k] == "fail":
                        success = False
                        print("Key " + key + " failed")
                    elif act_data[k] == ex_data[k]:
                        continue
                    else:
                        act = float(act_data[k])
                        ex = float(ex_data[k])
                        if abs((ex - act) / ex) > error:
                            success = False
                            print("Large change in " + k + " for " + key + ". Expected " + ex_data[k] + " / Actual: " + act_data[k])
                               
            else:
                success = False
                print("Key " + key + " missing in test results " + actual_filename)
        except KeyError:
            success = False
            print("Key " + key + " missing in test results " + actual_filename)  
        
    if len(expected_values) != len(actual_values):
        success = False
        print("Expected " + str(len(expected_values)) + " results. Found " + str(len(actual_values)) + " please update the test results csv")
    
    if success:
        print("Success!")
    elif fail_on_output:
        sys.exit(1)   
        