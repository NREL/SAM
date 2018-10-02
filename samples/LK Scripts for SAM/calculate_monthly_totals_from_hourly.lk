// This script defines a function that converts 8760 values to 12 monthly totals
// and returns 3x12 array of month names, days per month, and monthly totals.
// It then runs a simulation to generate test hourly data, and calls the function
// to demonstrate how to use the monthly data it generates.
//
// Written and tested in SAM 2016.3.14 in November 2016

hr_to_mo = define(hourly) {
	h_of_y = 0;
	d_per_m = [31,28,31,30,31,30,31,31,30,31,30,31];
	names = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];
	for ( m = 0; m < 12; m++ ) 	{
		monthly[m][2]=0;
		for ( d = 0; d < d_per_m[m]; d++ ) {
			for ( h = 0; h < 24; h++ )
			{
			monthly[m][0] = names[m];
			monthly[m][1] = d_per_m[m];
			monthly[m][2] = monthly[m][2] + hourly[h_of_y];
			h_of_y = h_of_y++;
			}
		}
	}
	return monthly;
};

out( 'Simulating...');
msg = '';
ok = simulate( 'msg' );
if ( !ok )
	outln( 'simulation failed with messages: \n\n' + msg );
elseif ( msg != '' )
	outln( 'simulation complete with messages: \n\n' + msg );
else
	outln( 'simulation complete.' );

outln();
hourly_output = get('gen');
monthly_output = hr_to_mo( hourly_output );

outln('Month\tDays\tkWh');
outln('-----------------------------------');
for( i=0; i<#monthly_output; i++ )
{
	outln(monthly_output[i][0] + '\t' + monthly_output[i][1] + '\t' + monthly_output[i][2] );
}

outln();
outln(monthly_output);

outln();
outln('Annual total: ' + sum(monthly_output) );
