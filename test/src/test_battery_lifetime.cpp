#include "gtest/gtest.h"
#include <shared\lib_battery.h>

class BatteryLifetimeTest : public ::testing::Test
{
protected:
	BatteryLifetimeTest()
	{
		_lifetime_matrix = new std::vector < double >;
		_lifetime_matrix->push_back(20); _lifetime_matrix->push_back(0); _lifetime_matrix->push_back(100);
		_lifetime_matrix->push_back(20); _lifetime_matrix->push_back(650); _lifetime_matrix->push_back(96);
		_lifetime_matrix->push_back(80); _lifetime_matrix->push_back(1500); _lifetime_matrix->push_back(87);
		_lifetime_matrix->push_back(80); _lifetime_matrix->push_back(0); _lifetime_matrix->push_back(100);
		_lifetime_matrix->push_back(80); _lifetime_matrix->push_back(150); _lifetime_matrix->push_back(96);
		_lifetime_matrix->push_back(80); _lifetime_matrix->push_back(300); _lifetime_matrix->push_back(87);
		util::matrix_t<double> batt_lifetime_matrix(6, 3, _lifetime_matrix);


		int replacement_option=0;
		double replacement_capacity;
		_lifetime = new lifetime_t(batt_lifetime_matrix, replacement_option, replacement_capacity);
	};
	virtual ~BatteryLifetimeTest()
	{
		delete [] _lifetime;
	};
	lifetime_t * _lifetime;
	std::vector<double> * _lifetime_matrix;
};

TEST_F(BatteryLifetimeTest, Cycling)
{
	// after initialized, no cycles
	EXPECT_EQ(0, _lifetime->cycles_elapsed());
}
