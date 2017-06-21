/*
* wxDVVariableStatistics.cpp
*
* This class Is a wxPanel that contains a table of statistics for the associated dataset
*/

#include <wx/scrolbar.h>
#include <wx/gbsizer.h>
#include <wx/tokenzr.h>
#include <wx/statline.h>
#include <wx/gdicmn.h>
#include <wx/filename.h>
#include <wx/clipbrd.h>
#include <wx/menu.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>
#include <wx/sstream.h>
#include <math.h>

#include "wex/dview/dvstatisticstablectrl.h"

#ifdef __WXMSW__
#include "wex/ole/excelauto.h"
#endif


//Tree Model Node

dvStatisticsTreeModelNode::dvStatisticsTreeModelNode(dvStatisticsTreeModelNode* parent, wxString nodeName)
{
	m_parent = parent;
	m_nodeName = nodeName;
	m_container = true;
}

dvStatisticsTreeModelNode::dvStatisticsTreeModelNode(dvStatisticsTreeModelNode* parent, wxString nodeName,
	double avg, double min, double max, double sum, double stdev, double avgdailymin, double avgdailymax)
{
	m_parent = parent;

	m_avg = avg;
	m_min = min;
	m_max = max;
	m_sum = sum;
	m_stdev = stdev;
	m_avgdailymin = avgdailymin;
	m_avgdailymax = avgdailymax;
	m_nodeName = nodeName;

	m_container = false;
}

dvStatisticsTreeModelNode::~dvStatisticsTreeModelNode()
{
	RemoveAllChildren();
}

bool dvStatisticsTreeModelNode::IsContainer() const 
{ 
	return m_container; 
}

dvStatisticsTreeModelNode* dvStatisticsTreeModelNode::GetParent() 
{ 
	return m_parent; 
}

std::vector<dvStatisticsTreeModelNode*> dvStatisticsTreeModelNode::GetChildren()
{ 
	return m_children; 
}

dvStatisticsTreeModelNode* dvStatisticsTreeModelNode::GetNthChild(unsigned int n) 
{ 
	if (n >= m_children.size()) { return NULL; }
	return m_children[n]; 
}

void dvStatisticsTreeModelNode::Append(dvStatisticsTreeModelNode* child) 
{ 
	m_children.push_back(child);
}

unsigned int dvStatisticsTreeModelNode::GetChildCount() const 
{ 
	return m_children.size();
}

void dvStatisticsTreeModelNode::RemoveAllChildren()
{
	m_children.clear();
}

wxString dvStatisticsTreeModelNode::GetName()
{
	return m_nodeName;
}

double dvStatisticsTreeModelNode::GetMean()
{
	return m_avg;
}

double dvStatisticsTreeModelNode::GetMin()
{
	return m_min;
}

double dvStatisticsTreeModelNode::GetMax()
{
	return m_max;
}

double dvStatisticsTreeModelNode::GetSum()
{
	return m_sum;
}

double dvStatisticsTreeModelNode::GetStDev()
{
	return m_stdev;
}

double dvStatisticsTreeModelNode::GetAvgDailyMin()
{
	return m_avgdailymin;
}

double dvStatisticsTreeModelNode::GetAvgDailyMax()
{
	return m_avgdailymax;
}


//Tree Model

dvStatisticsTreeModel::dvStatisticsTreeModel()
{
	m_root = new dvStatisticsTreeModelNode(NULL, "All");
}

int dvStatisticsTreeModel::Compare(const wxDataViewItem &item1, const wxDataViewItem &item2, unsigned int column, bool ascending) const
{
	wxASSERT(item1.IsOk() && item2.IsOk());
	// should never happen

	if (IsContainer(item1) && IsContainer(item2))
	{
		wxVariant value1, value2;
		GetValue(value1, item1, 0);
		GetValue(value2, item2, 0);

		wxString str1 = value1.GetString();
		wxString str2 = value2.GetString();
		int res = str1.Cmp(str2);
		if (res) return res;

		// items must be different
		wxUIntPtr litem1 = (wxUIntPtr)item1.GetID();
		wxUIntPtr litem2 = (wxUIntPtr)item2.GetID();

		return litem1 - litem2;
	}

	return wxDataViewModel::Compare(item1, item2, column, ascending);
}

wxString dvStatisticsTreeModel::GetColumnType(unsigned int col) const
{
	if (col == 0)
	{
		return wxT("string");
	}
	else
	{
		return wxT("double");
	}
}

void dvStatisticsTreeModel::GetValue(wxVariant &variant, const wxDataViewItem &item, unsigned int col) const
{
	wxASSERT(item.IsOk());

	dvStatisticsTreeModelNode *node = (dvStatisticsTreeModelNode*)item.GetID();
	switch (col)
	{
	case 0:
		variant = node->GetName();
		break;
	case 1:
		variant = node->GetMean();
		break;
	case 2:
		variant = node->GetMin();
		break;
	case 3:
		variant = node->GetMax();
		break;
	case 4:
		variant = node->GetSum();
		break;
	case 5:
		variant = node->GetStDev();
		break;
	case 6:
		variant = node->GetAvgDailyMin();
		break;
	case 7:
		variant = node->GetAvgDailyMax();
		break;
	default:
		break;
	}
}

bool dvStatisticsTreeModel::SetValue(const wxVariant &variant, const wxDataViewItem &item, unsigned int col)
{
	//wxASSERT(item.IsOk());

	//dvStatisticsTreeModelNode *node = (dvStatisticsTreeModelNode*)item.GetID();
	//switch (col)
	//{
	//case 0:
	//	node->m_title = variant.GetString();
	//	return true;
	//case 1:
	//	node->m_artist = variant.GetString();
	//	return true;
	//case 2:
	//	node->m_year = variant.GetLong();
	//	return true;
	//case 3:
	//	node->m_quality = variant.GetString();
	//	return true;
	//default:
	//	break;
	//}

	return false;
}

wxDataViewItem dvStatisticsTreeModel::GetParent(const wxDataViewItem &item) const
{
	// the invisible root node has no parent
	if (!item.IsOk())
		return wxDataViewItem(0);

	dvStatisticsTreeModelNode *node = (dvStatisticsTreeModelNode*)item.GetID();

	// "MyMusic" also has no parent
	if (node == m_root)
		return wxDataViewItem(0);

	return wxDataViewItem((void*)node->GetParent());
}

bool dvStatisticsTreeModel::IsContainer(const wxDataViewItem &item) const
{
	// the invisble root node can have children
	if (!item.IsOk())
		return true;

	dvStatisticsTreeModelNode *node = (dvStatisticsTreeModelNode*)item.GetID();
	return node->IsContainer();
}

unsigned int dvStatisticsTreeModel::GetChildren(const wxDataViewItem &parent, wxDataViewItemArray &array) const
{
	dvStatisticsTreeModelNode *node = (dvStatisticsTreeModelNode*)parent.GetID();

	if (!node)
	{
		array.Add(wxDataViewItem((void*)m_root));
		return 1;
	}

	if (node->GetChildCount() == 0)
	{
		return 0;
	}

	unsigned int count = node->GetChildren().size();
	for (unsigned int pos = 0; pos < count; pos++)
	{
		dvStatisticsTreeModelNode *child = node->GetChildren()[pos];
		array.Add(wxDataViewItem((void*)child));
	}

	return count;
}

void dvStatisticsTreeModel::Refresh(std::vector<wxDVVariableStatistics*> stats, bool showMonths)
{
	wxDVStatisticsDataSet *ds;
	dvStatisticsTreeModelNode *groupNode;
	dvStatisticsTreeModelNode *variableNode;
	dvStatisticsTreeModelNode *monthNode;
	StatisticsPoint p;
	wxString groupName = "";

	//Clear existing nodes
	if (m_root == NULL)
	{
		m_root = new dvStatisticsTreeModelNode(NULL, "All");
	}
	else
	{
		m_root->RemoveAllChildren();
	}

	//Repopulate nodes, organizing them by group
	for (int i = 0; i < stats.size(); i++)
	{
		groupName = "";
		for (int j = 0; j < m_root->GetChildCount(); j++)
		{
			if (m_root->GetNthChild(j)->GetName() == stats[i]->GetGroupName())
			{
				groupNode = m_root->GetNthChild(j);
				groupName = groupNode->GetName();
				break;
			}
		}

		if (groupName == "") 
		{ 
			groupName = stats[i]->GetGroupName();
			groupNode = new dvStatisticsTreeModelNode(m_root, groupName); 
			m_root->Append(groupNode);
		}

		ds = stats[i]->GetDataSet();

		if (showMonths)
		{
			variableNode = new dvStatisticsTreeModelNode(groupNode, ds->GetSeriesTitle() + " (" + ds->GetUnits() + ")");

		for (int j = 0; j < ds->Length(); j++)
		{
			p = ds->At(j);
			monthNode = new dvStatisticsTreeModelNode(variableNode, p.name, p.Mean, p.Min, p.Max, p.Sum, p.StDev, p.AvgDailyMin, p.AvgDailyMax);
			variableNode->Append(monthNode);
		}

			groupNode->Append(variableNode);
		}
		else
		{
			for (int j = 0; j < ds->Length(); j++)
			{
				if (ds->At(j).name == "Total")
				{
					p = ds->At(j);
					variableNode = new dvStatisticsTreeModelNode(groupNode, ds->GetSeriesTitle() + " (" + ds->GetUnits() + ")", p.Mean, p.Min, p.Max, p.Sum, p.StDev, p.AvgDailyMin, p.AvgDailyMax);
					groupNode->Append(variableNode);
					break;
				}
			}
		}
	}
}

wxDataViewItem dvStatisticsTreeModel::GetRoot()
{
	return (wxDataViewItem)m_root;
}


//wxDVVariableStatistics

wxDVVariableStatistics::wxDVVariableStatistics(wxDVStatisticsDataSet *ds, wxString GroupName, bool OwnsDataset)
: m_data(ds)
{
	m_ownsDataset = OwnsDataset;
	m_groupName = GroupName;
}

wxDVVariableStatistics::~wxDVVariableStatistics()
{
	if (m_ownsDataset)
	{
		delete m_data;
	}
}

StatisticsPoint wxDVVariableStatistics::At(size_t i, double m_offset, double m_timestep) const
{
	StatisticsPoint p = StatisticsPoint();

	if ((i < m_data->Length()) && (i >= 0))
	{
		p.x = m_data->At(i).x;
		p.Sum = m_data->At(i).Sum;
		p.Min = m_data->At(i).Min;
		p.Max = m_data->At(i).Max;
		p.Mean = m_data->At(i).Mean;
		p.StDev = m_data->At(i).StDev;
		p.AvgDailyMin = m_data->At(i).AvgDailyMin;
		p.AvgDailyMax = m_data->At(i).AvgDailyMax;
	}
	else
	{
		p.x = m_offset + (i * m_timestep);
		p.Sum = 0.0;
		p.Min = 0.0;
		p.Max = 0.0;
		p.Mean = 0.0;
		p.StDev = 0.0;
		p.AvgDailyMin = 0.0;
		p.AvgDailyMax = 0.0;
	}

	return p;
}

wxString wxDVVariableStatistics::GetGroupName()
{
	return m_groupName;
}


//wxDVStatisticsTableCtrl

enum { ID_COPY_DATA_CLIP = wxID_HIGHEST + 1251, ID_SAVE_DATA_CSV, ID_SEND_EXCEL };

BEGIN_EVENT_TABLE(wxDVStatisticsTableCtrl, wxPanel)
	EVT_MENU_RANGE(ID_COPY_DATA_CLIP, ID_SEND_EXCEL, wxDVStatisticsTableCtrl::OnPopupMenu)
	EVT_CHECKBOX(wxID_ANY, wxDVStatisticsTableCtrl::OnShowMonthsClick)
	END_EVENT_TABLE()

wxDVStatisticsTableCtrl::wxDVStatisticsTableCtrl(wxWindow *parent, wxWindowID id)
	: wxPanel(parent, id)
{
	m_showMonths = false;

	m_ctrl = new wxDataViewCtrl(this, ID_STATISTICS_CTRL, wxDefaultPosition, wxSize(1040, 720), 
		wxDV_MULTIPLE | wxDV_ROW_LINES | wxDV_VERT_RULES | wxDV_HORIZ_RULES | wxBORDER_NONE);
	m_ctrl->Bind(wxEVT_COMMAND_DATAVIEW_ITEM_CONTEXT_MENU, &wxDVStatisticsTableCtrl::OnContextMenu, this);

	m_StatisticsModel = new dvStatisticsTreeModel();
	m_chkShowMonths = new wxCheckBox(this, wxID_ANY, "Show Monthly Values", wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT);

	wxBoxSizer *top_sizer = new wxBoxSizer(wxVERTICAL);
//	top_sizer->Add(m_chkShowMonths, 0, wxALL | wxALIGN_CENTER_VERTICAL, 4);
	top_sizer->Add(m_chkShowMonths, 0, wxALL , 4);
	top_sizer->Add(m_ctrl, 1, wxALL | wxEXPAND, 0);
	SetSizer(top_sizer);

	m_contextMenu.Append(ID_COPY_DATA_CLIP, "Copy data to clipboard");
	m_contextMenu.Append(ID_SAVE_DATA_CSV, "Save data to CSV...");
#ifdef __WXMSW__
	m_contextMenu.Append(ID_SEND_EXCEL, "Send data to Excel...");
#endif
}

wxDVStatisticsTableCtrl::~wxDVStatisticsTableCtrl(void)
{
	RemoveAllDataSets();
}

void wxDVStatisticsTableCtrl::Invalidate()
{
	//m_plotSurface->Invalidate();
	//m_plotSurface->Refresh();
}

void wxDVStatisticsTableCtrl::RebuildDataViewCtrl()
{
	wxDataViewTextRenderer *tr;

	m_StatisticsModel->Refresh(m_variableStatistics, m_showMonths);
	m_ctrl->ClearColumns();
	m_ctrl->AssociateModel(m_StatisticsModel.get());

	//TODO:  Is there a way to highlight "Total" nodes (i.e. bold, background color, etc...)?

	tr = new wxDataViewTextRenderer("string", wxDATAVIEW_CELL_INERT, wxALIGN_LEFT);
	wxDataViewColumn *column0 = new wxDataViewColumn("", tr, 0, 350, wxALIGN_LEFT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column0);

	tr = new wxDataViewTextRenderer("double", wxDATAVIEW_CELL_INERT, wxALIGN_RIGHT);
	wxDataViewColumn *column1 = new wxDataViewColumn("Mean", tr, 1, 110, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column1);

	tr = new wxDataViewTextRenderer("double", wxDATAVIEW_CELL_INERT, wxALIGN_RIGHT);
	wxDataViewColumn *column2 = new wxDataViewColumn("Min", tr, 2, 110, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column2);

	tr = new wxDataViewTextRenderer("double", wxDATAVIEW_CELL_INERT, wxALIGN_RIGHT);
	wxDataViewColumn *column3 = new wxDataViewColumn("Max", tr, 3, 110, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column3);

	tr = new wxDataViewTextRenderer("double", wxDATAVIEW_CELL_INERT, wxALIGN_RIGHT);
	wxDataViewColumn *column4 = new wxDataViewColumn("Sum", tr, 4, 110, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column4);

	tr = new wxDataViewTextRenderer("double", wxDATAVIEW_CELL_INERT, wxALIGN_RIGHT);
	wxDataViewColumn *column5 = new wxDataViewColumn("Std Dev", tr, 5, 110, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column5);

	tr = new wxDataViewTextRenderer("double", wxDATAVIEW_CELL_INERT, wxALIGN_RIGHT);
	wxDataViewColumn *column6 = new wxDataViewColumn("Avg Daily Min", tr, 6, 110, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column6);

	tr = new wxDataViewTextRenderer("double", wxDATAVIEW_CELL_INERT, wxALIGN_RIGHT);
	wxDataViewColumn *column7 = new wxDataViewColumn("Avg Daily Max", tr, 7, 110, wxALIGN_RIGHT, wxDATAVIEW_COL_RESIZABLE);
	m_ctrl->AppendColumn(column7);

	wxDataViewItem item = m_StatisticsModel->GetRoot();
	if (item.IsOk()) { m_ctrl->Expand(item); }

}

void wxDVStatisticsTableCtrl::AddDataSet(wxDVTimeSeriesDataSet *d)
{
	wxDVStatisticsDataSet *s = new wxDVStatisticsDataSet(d);
	wxDVVariableStatistics *p = new wxDVVariableStatistics(s, d->GetGroupName(), true);
	m_variableStatistics.push_back(p); //Add to data sets list.
}

bool wxDVStatisticsTableCtrl::RemoveDataSet(wxDVTimeSeriesDataSet *d)
{
	//wxDVVariableStatistics *plotToRemove = NULL;
	wxDVStatisticsDataSet *ds;
	int removedIndex = 0;

	//Find the plottable:
	for (size_t i = 0; i < m_variableStatistics.size(); i++)
	{
		ds = m_variableStatistics[i]->GetDataSet();

		if (ds->IsSourceDataset(d))
		{
			removedIndex = i;
			//plotToRemove = m_variableStatistics[i];
			break;
		}
	}

	//if (!plotToRemove)
	//	return false;

	//for (int i = 0; i<wxPLPlotCtrl::NPLOTPOS; i++)
	//	m_plotSurface->RemovePlot(plotToRemove);

	m_variableStatistics.erase(m_variableStatistics.begin() + removedIndex); //This is more efficient than remove when we already know the index.

	RebuildDataViewCtrl();

	return true;
}

void wxDVStatisticsTableCtrl::RemoveAllDataSets()
{
	//Remove all data sets. Deleting a data set also deletes its plottable.
	for (size_t i = 0; i < m_variableStatistics.size(); i++)
		delete m_variableStatistics[i];

	m_variableStatistics.clear();

	RebuildDataViewCtrl();
}

void wxDVStatisticsTableCtrl::WriteDataAsText(wxUniChar sep, wxOutputStream &os, bool visible_only, bool include_x)
{
	if (m_variableStatistics.size() == 0) { return; }

	wxTextOutputStream tt(os);
	wxString sepstr(sep);
	wxDVStatisticsDataSet* stats;
	StatisticsPoint stat;

	//Add column headers
	tt << wxString("Variable");
	tt << sepstr;
	tt << wxString("Time");
	tt << sepstr;
	tt << wxString("Mean");
	tt << sepstr;
	tt << wxString("Min");
	tt << sepstr;
	tt << wxString("Max");
	tt << sepstr;
	tt << wxString("Sum");
	tt << sepstr;
	tt << wxString("St Dev");
	tt << sepstr;
	tt << wxString("Avg Daily Min");
	tt << sepstr;
	tt << wxString("Avg Daily Max");
	tt << "\n";

	//Add data
	for (size_t i = 0; i < m_variableStatistics.size(); i++)
	{
		stats = m_variableStatistics[i]->GetDataSet();

		for (size_t j = 0; j < stats->Length(); j++)
		{
			stat = stats->At(j);
			if (j == 0) { tt << m_variableStatistics[i]->GetGroupName() + wxString(":") + stats->GetSeriesTitle() + " (" + stats->GetUnits() + ")"; }
			tt << sepstr;
			tt << stat.name;
			tt << sepstr;
			tt << stat.Mean;
			tt << sepstr;
			tt << stat.Min;
			tt << sepstr;
			tt << stat.Max;
			tt << sepstr;
			tt << stat.Sum;
			tt << sepstr;
			tt << stat.StDev;
			tt << sepstr;
			tt << stat.AvgDailyMin;
			tt << sepstr;
			tt << stat.AvgDailyMax;
			tt << "\n";
		}
	}
}

void wxDVStatisticsTableCtrl::OnCollapse(wxCommandEvent& WXUNUSED(event))
{
	wxDataViewItem item = m_ctrl->GetSelection();
	if (item.IsOk())
		m_ctrl->Collapse(item);
}

void wxDVStatisticsTableCtrl::OnExpand(wxCommandEvent& WXUNUSED(event))
{
	wxDataViewItem item = m_ctrl->GetSelection();
	if (item.IsOk())
		m_ctrl->Expand(item);
}

void wxDVStatisticsTableCtrl::OnContextMenu(wxDataViewEvent &event)
{
	PopupMenu(&m_contextMenu);
}

void wxDVStatisticsTableCtrl::OnPopupMenu(wxCommandEvent &evt)
{
	int menuid = evt.GetId();

	switch (menuid)
	{
		case ID_COPY_DATA_CLIP:
			if (wxTheClipboard->Open())
			{
				wxString text;
				wxStringOutputStream sstrm(&text);

				WriteDataAsText('\t', sstrm);
				wxTheClipboard->SetData(new wxTextDataObject(text));
				wxTheClipboard->Close();
			}

			break;

#ifdef __WXMSW__
			case ID_SEND_EXCEL:
				{
					wxExcelAutomation xl;

					if (!xl.StartExcel())
					{
						wxMessageBox("Could not start Excel.");
						return;
					}

					xl.Show(true);

					if (!xl.NewWorkbook())
					{
						wxMessageBox("Could not create a new Excel worksheet.");
						return;
					}

					wxString text;
					wxStringOutputStream sstrm(&text);
					WriteDataAsText('\t', sstrm);

					xl.PasteNewWorksheet("Plot Data", text);
					xl.AutoFitColumns();
				}

				break;
#endif

				case ID_SAVE_DATA_CSV:
					{
						wxFileDialog fdlg(this, "Save Graph Data", "", "graphdata", "CSV Data Files (*.csv)|*.csv", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

						if (fdlg.ShowModal() == wxID_OK)
						{
							wxString fn = fdlg.GetPath();

							if (fn != "")
							{
								//Make sure we have an extension
								wxString ext;

								wxFileName::SplitPath(fn, NULL, NULL, NULL, &ext);
								if (ext.Lower() != "csv") { fn += ".csv"; }

								wxFFileOutputStream out(fn);

								if (out.IsOk())
								{
									WriteDataAsText(',', out);
								}
								else
								{
									wxMessageBox("Could not write to file: \n\n" + fn, "Save Error", wxICON_ERROR);
								}
							}
						}
					}

					break;
	}
}

void wxDVStatisticsTableCtrl::OnShowMonthsClick(wxCommandEvent &e)
{
	m_showMonths = m_chkShowMonths->GetValue();
	RebuildDataViewCtrl();
}
