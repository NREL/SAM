///////////////////////////////////////////////////////////////////////////////
// Name:        pdfcoonspatchmesh.h
// Purpose:     
// Author:      Ulrich Telle
// Modified by:
// Created:     2009-06-24
// Copyright:   (c) Ulrich Telle
// Licence:     wxWindows licence
///////////////////////////////////////////////////////////////////////////////

/// \file pdfcoonspatchmesh.h Interface of the wxPdfCoonsPatchMesh class

#ifndef _PDF_COONS_PATCH_MESH_H_
#define _PDF_COONS_PATCH_MESH_H_

// wxWidgets headers
#include <wx/dynarray.h>

// wxPdfDocument headers
#include "wex/pdf/pdfdocdef.h"
#include "wex/pdf/pdfcolour.h"

/// Class representing a coons patch mesh.
class WXDLLIMPEXP_PDFDOC wxPdfCoonsPatchMesh
{
public:
  /// Constructor
  wxPdfCoonsPatchMesh();

  /// Destructor
  virtual ~wxPdfCoonsPatchMesh();

  /// Add patch to mesh
  /**
  * \param edgeFlag flag indicating the patch position relative to previous patches
  *   \li 0 - new patch, unrelated to previous patches (the first patch added must have this flag)
  *   \li 1 - above previous patch
  *   \li 2 - right to previous patch
  *   \li 3 - below previous patch
  * \param colours array of colours of this patch (size: 4 if edge flag is 1, 2 otherwise)
  * \param x array of x coordinates of patch mesh points (size: 12 if edge flag is 1, 8 otherwise)
  * \param y array of y coordinates of patch mesh points (size: 12 if edge flag is 1, 8 otherwise)
  * \return true if the added patch is valid
  */
  bool AddPatch(int edgeFlag, wxPdfColour colours[], double x[], double y[]);

  /// Checks whether the coons patch mesh is valid
  /**
  * \return TRUE if the coons patch mesh is valid, FALSE otherwise
  */
  bool Ok() const { return m_ok; }

  /// Get colour type of the coons patch mesh
  /**
  * \return the colour type of the coons patch mesh (gray scale, RGB or CMYK)
  */
  wxPdfColourType GetColourType() const { return m_colourType; }

  /// Get the number of patches
  /**
  * \return the number of patches of the coons patch mesh
  */
  size_t GetPatchCount() const { return m_patches.size(); }

  /// Get the array of patches
  /**
  *
  * \return array of patches
  */
  const wxArrayPtrVoid* GetPatches() const { return &m_patches; }

private:
  bool            m_ok;         ///< flag whether the coons patch mesh is valid
  wxPdfColourType m_colourType; ///< colour type of the mesh
  wxArrayPtrVoid  m_patches;    ///< array of patches
};

#endif
