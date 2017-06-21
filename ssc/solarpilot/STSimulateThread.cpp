#include "STSimulateThread.h"
#include "definitions.h"
#include "exceptions.hpp"

#ifdef SP_USE_SOLTRACE
using namespace std;

int STSimThread::GetResultCode(){ return ResultCode; }

st_context_t STSimThread::GetContextId() { return ContextId; }

void STSimThread::Setup( st_context_t spcxt, int thd_num, int seed, bool is_load_st0, bool is_save_st0 )
{
	ThreadNum = thd_num;
	CancelFlag = false;
	Finished = false;
	SeedVal = seed;
	ContextId = spcxt;
    LoadStage0Data = is_load_st0;
    SaveStage0Data = is_save_st0;
	ResultCode = -1;
	NToTrace = 0;
	NTraced = 0; 
	NTraceTotal = 0;
	CurStage = 0;
	NStages = 0;
}

void STSimThread::CopyStageRayData( vector<vector<double> > &src, int which_stage /*0 or 1*/, int istart, int iend )
{
    vector<vector<double> > *which_raydat;
    if(which_stage==0)
        which_raydat = &raydata_st0;
    else
        which_raydat = &raydata_st1;

    which_raydat->clear();
    try{
        which_raydat->reserve(iend-istart);
    }
    catch(std::exception &e)
    {
        string msg = e.what();
        msg.append(": Error resizing raytrace data array");
        throw spexception(msg.c_str());
    }

    for(int i=istart; i<iend; i++)
        which_raydat->push_back(src.at(i));

}

vector<vector< double > > *STSimThread::GetStage0RayDataObject()
{
    return &raydata_st0;
}

vector<vector< double > > *STSimThread::GetStage1RayDataObject()
{
    return &raydata_st1;
}

STSimThread::~STSimThread()
{
	::st_free_context( ContextId );
}	


void STSimThread::CancelTrace()
{
	CancelLock.lock();
	CancelFlag = true;
	CancelLock.unlock();
}

bool STSimThread::IsTraceCancelled()
{
	bool r;
	CancelLock.lock();
	r = CancelFlag;
	CancelLock.unlock();
	return r;
}

bool STSimThread::IsFinished()
{
	bool f;
	FinishedLock.lock();
	f = Finished;
	FinishedLock.unlock();
	return f;
}

void STSimThread::UpdateStatus(int ntracedtotal, int ntraced, int ntotrace, int curstage, int nstages)
{
	StatusLock.lock();
	this->NTraceTotal = ntracedtotal;
	this->NTraced = ntraced;
	this->NToTrace = ntotrace;
	this->CurStage = curstage;
	this->NStages = nstages;
	StatusLock.unlock();
}

void STSimThread::GetStatus(int *total, int *traced, int *ntotrace, int *stage, int *nstages)
{
	StatusLock.lock();
	*total = this->NTraceTotal;
	*traced = this->NTraced;
	*ntotrace = this->NToTrace;
	*stage = this->CurStage;
	*nstages = this->NStages;
	StatusLock.unlock();
}

//void *STSimThread::Entry()
//{
//	ResultCode = st_sim_run( ContextId, (unsigned int)SeedVal, STCallback_MT, (void*) this );
//	FinishedLock.lock();
//	Finished = true;
//	FinishedLock.unlock();
//	return NULL;
//
//};

void STSimThread::StartThread()
{
    //if(LoadStage0Data)
        //ResultCode = st_sim_run_data(ContextId, (unsigned int)SeedVal, &raydata_st0, &raydata_st1, false, STCallback_MT, (void*) this);
    //else if(SaveStage0Data)
        //ResultCode = st_sim_run_data(ContextId, (unsigned int)SeedVal, &raydata_st0, &raydata_st1, true, STCallback_MT, (void*) this);
    //else
	    ResultCode = st_sim_run( ContextId, (unsigned int)SeedVal, true, STCallback_MT, (void*) this );

	FinishedLock.lock();
	Finished = true;
	FinishedLock.unlock();
	//	return NULL;

};	

#endif
