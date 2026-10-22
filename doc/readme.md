# SAM Help Build Guide

These instructions are for writing and editing content and building SAM's Help system.

Help content is written in the reStructuredText (.rst) text markup language: https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html.

The Sphinx documentation generator converts .rst source files into structured HTML.

Sphinx is a Python package: https://www.sphinx-doc.org/en/master/.

SAM Help uses Sphinx Book Theme to apply formatting to the HTML: https://sphinx-book-theme.readthedocs.io/en/stable/index.html.

Complete documentation for the Sphinx Book Theme, Sphinx, and reStructuredText is available at the links above. The documentation below describes the application of these tools to build SAM Help.

## Requirements

* Python
* A virtual environment like `venv` or `conda` (recommended)
* Python packages listed in `requirements.txt`
* Make  (optional)
* A good text editor that can open a folder as a workspace and has syntax highlighting for reStructuredText (VS Code works well)

Tested on Windows with Python 3.12.10 and GNU Make 4.4.1.

## Build Instructions

1. Set up a virtual environment and install the packages listed in requirements.txt.

   You only need to build the virtual environment once. After it is built, you can start with Step 2 to build Help.

2. Open a terminal window and go to the SAM doc folder.

   ```
   cd path/to/sam_dev/SAM/doc 
   ```

3. Activate the Python environment.

   For example, using Python `venv`:

   ```
   path/to/sam-help-venv/Scripts/activate
   ```

3. Build Help from the `path/to/sam_dev/SAM/doc` folder.

   ```
   sphinx-build -M html source build
   ```

   If there are changes to table of contents or other structural changes, clean the build first:

   Windows:

   ```
   rmdir /s /q build
   ```

   Mac or Linux:

   ```
   rm -rf build
   ```

   As an alternative, run Make, assuming Make is installed (see below) and `Makefile` is in the `doc` folder.

   ```
   make html
   ```

   To clean the build using Make, run `make clean`.

   To see a list of Make targets, run `make`.

4. If there are any build errors, fix them by editing the appropriate .rst file(s).

5. When the build finishes, HTML and associated files should be in the `../doc/build/html` folder. (The `../doc/build/doctrees` folder is used during the build and not needed to display the HTML files.)

To see Help, open the `../doc/html/index.html` file.

### Make Installation Instructions (Optional)

For Linux, Make comes with the operating system, so you do not need to install it.

For macOS, Make is part of the Command Line Tools that are required to build SAM from source. See [Installing the command-line tools](https://developer.apple.com/documentation/xcode/installing-the-command-line-tools/) if you need to install them.

For Windows:

1. Download the "Complete package, except sources" from https://gnuwin32.sourceforge.net/packages/make.htm.

2. Run the make installer (`make-3.81.exe`).

3. Add the make installation folder `C:\Program Files (x86)\GnuWin32\bin` to the Windows `Path` user environment variable.

## Help Context IDs in SAM

SAM uses Help Context IDs to open context-specific Help files from the SAM user interface.

The Help context ID is a string "chapter-name/topic_name" that points to the folder and file name of the HTML for the Help topic. For example, the Help topic for the behind-the-meter Battery Dispatch input page is "battery-storage/battery_dispatch_btm".

The Help context ID is the path to the HTML file without the `.html` extension. The extension is added by the `ShowHelp()` function in `SAM/main.cpp`.

Help context IDs are defined in different places in the SAM code depending on the context.

### SAM Input Pages

Help context IDs for SAM input pages are defined in `startup.lk`. The `addpage()` function for the input page "help" parameter points to the folder and file name of the HMTL file for the page's Help topic.

For example, for the **Battery Dispatch** page for behind the meter batteries, the help id is defined by the `'help' = 'battery-storage/battery_dispatch_btm'` parameter:

```
addpage( [[ {'name'='Battery Dispatch Peak Shaving BTM', 'caption'='Peak Shaving'} ],
	      [ {'name'='Battery Dispatch Grid Power Targets', 'caption'='Grid Power Targets'} ],
		  [ {'name'='Battery Dispatch Battery Power Targets', 'caption'='Battery Power Targets'}],
		  [ {'name'='Battery Dispatch Manual', 'caption'='Manual'}],
		  [ {'name'='Battery Dispatch Retail Rate', 'caption'='Retail Rate'} ]],
		  { 'sidebar'='Battery Dispatch', 
            'help'='battery-storage/battery_dispatch_btm',
			'exclusive_var' = 'batt_dispatch_excl',
			'exclusive_header_pages' = ['Battery Dispatch Common', 'Battery Dispatch Standalone Options BTM'], 
            'exclusive_tabs'=true, 'exclusive_hide'=true, 'bin_name'='Battery' } );
```

### SAM Windows

Help IDs for SAM windows are defined by the event handler in the window definition.

For example, for the "Combine Cases" window (`SAM/combinecases.cpp`):

```
void CombineCasesDialog::OnEvt(wxCommandEvent& e)
{
	switch (e.GetId())
	{
		case wxID_HELP:
			SamApp::ShowHelp("window-reference/win_combine_cases");
			break;
		case ID_chlCases:
			{
...
}
```

For the "Edit Losses" window (`SAM/lossadj.cpp`):

```
	void OnCommand( wxCommandEvent &e )
	{
		switch( e.GetId() )
		{
		case wxID_HELP:
			SamApp::ShowHelp("window-reference/win_edit_losses");
			break;
...
```

To find all SAM window Help IDs, search the SAM project for "wxID_HELP", or more specifically "case wxID_HELP" and "if (evt.GetId() == wxID_HELP)".

## Checking for Broken Links

Use the [linkcheck builder](https://www.sphinx-doc.org/en/master/usage/builders/index.html#sphinx.builders.linkcheck.CheckExternalLinksBuilder) to check for broken external links. (Errors in the build process described above should find broken internal links.):

From `/SAM/doc/source`, run

```
sphinx-build -b linkcheck . _build/linkcheck
```

This will generate a report of broken links in `/SAM/doc/source/_build/linkcheck/output.txt`.

## File Structure

SAM's Help system is organized into chapters like "Introduction", "Getting Started", etc. Each model has its own chapter like "Detailed Photovoltaic Model".

All files required to build the SAM Help system are in the `../doc/source` folder.

Topic files are `.rst` text files that contain text for a topic. Each SAM input page has its own topic file. Other topic files are not associated with a SAM input page, for example the "Getting Started" and "SAM CSV Format for Solar" topics.

Folders contain topic files for a chapter. For example, topic files for the "Detailed Photovoltaic Model" chapter are in the `..doc/source/detailed-photovoltaic-model` folder.

Folder names use hyphens (-) to separate words, for example `detailed-photovoltaic-model`.

Topic file names use underscores (_) to separate words, for example `pv_sizing_instructions.rst`.

In addition to a folder for each chapter, the `source` folder contains the following folders:

* `../doc/source/images` contains .png image files.

* `../doc/source/archive` contains source files for models that were removed from SAM. These topics can be added back in to the Help system if the models are ever reinstated in SAM.

## Theme Configuration

SAM Help uses the Sphinx Book theme. There are many other themes available. The theme is set by the `html_theme` option in the `conf.py` file. Options specific to the Sphinx Book theme are set by `html_theme_options`.

The `conf.py` file contains configurations settings for the Sphinx documentation project.

The `../_static/custom.css` file contains a few custom styles that modify the default Sphinx Book Theme styles. 

## Table of Contents

SAM Help has a main table of contents that appears to the left of the topic content, and chapter tables of contents that appear at the right of the topic content.

Sphinx builds the main table of contents from the `toctree` directive in the `../source/index.rst` file.

For each chapter, Sphinx uses the `toctree` directive in the `../source/chapter-title/index.rst` file to build the table of contents for that chapter.

If the browser window displaying Help is too narrow to display the table of contents, it hides them so that only the content is visible.

This folder structure and behavior is specific to Sphinx Book Theme and may work differently for a different theme.

## Editing Content

Use text editor to edit the topic content `.rst` files in the `..doc/source`.

To add a new chapter:

1. Create a new folder with hyphen-separated words.
2. Create a section for the chapter in `../source/index.rst`.
3. Create a new `index.rst` file in the new chapter's folder. Use the index file in other folders as an example.

To add a new topic to a chapter:

1. Create a new `.rst` file with underscore-separated words in the appropriate folder.
2. Add a topic title to the file underlined with "=". The length of the underline must equal the length of the title.
3. Add the file to the chapter's `../source/chapter-name/index.rst` file so that it appears in the chapter's table of contents.
4. Add the file to the appriopriate section of `../source/index.rst` so that it appears in the main table of contents.

## Archiving a Chapter

When a model or feature that has a Help chapter is removed from SAM, archive the chapter:

1. Move the folder to the `../source/archive` folder.
2. Remove the chapter's section from the main table of contents in `..source/index.rst`.
3. Use Find in Files to remove any cross references to files in the archived chapter by searching for file names and any targets (`.. _target-name`) in those files.

## Includes

The `../doc/includes` folder contains files with text "snippets" or reusable text.

For example the `../doc/includes/snip_system_availability.rst` is a description of the system availability losses that is used in descriptions of each of the performance models, such as in the Detailed PV model's "Electrical Losses" topic, and the Physical Trough model's "System Control" topic.

To insert the content of `snip_system_availability.rst` from a file in a `../doc/source` subfolder:

```
.. include:: ../includes/snip_system_availability.rst
```

If possible, avoid using headings in snippet files to avoid inconsistencies between the heading level in the snippet file and the file it is being inserted to. *Check this: The Sphinx documentation suggests document structure is preserved if the heading order is the same as the document containing the `include` directive: "If an included document fragment contains section structure, the title adornments must match those of the master document" from https://docutils.sourceforge.io/docs/ref/rst/directives.html#include*.

**Important note about snippets**: Do not include targets (`.. _target-name-in-snippet`), cross references, references to targets in other snippet files, or cross references to other snippet files in snippet files. Doing so results in duplicate target names which breaks the Sphinx build. You can include cross references in snippet files to topic files that are not in the `../doc/includes` folder.

## Headings

Limit section headings to three levels.

In reStructuredText, headings are identified by underline characters (https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#sections). The character used to underline the text determines the heading level based on the "succession of headings" in each file. This means that each file can use different characters to represent heading levels.

For SAM Help, the convention is to use the order defined below for heading levels. However, if a given file uses a different order, for example (~,-,'), then, for that  file an underline of "~" would be for Heading 1, "-" for Heading 2, and "'" for Heading 3, even if other files use the convention below.

```
Heading 1: Topic Title
======================

The topic title goes at the top of the file and is used to construct tables of contents and as text for cross-reference hyperlinks. No text should precede the topic title.

Heading 2: Section Title
~~~~~~~~~~~~~~~~~~~~~~~~

The section title is for sections of the topic and for labels of group boxes in the user interface.

Heading 3: Subsection Title
---------------------------

A subsection title correlates with divider labels in the user interface.

Heading 2: Another Section Title
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The "~" character now represents Heading 2 because of the order above.
```

## Lists and Step-by-step Instructions

Use `#` to automatically number lists. Use `*` for bulleted lists. Lines in a list can be separated by a blank line or not.

For the title of step-by-step instructions, use **bold** text (`**bold**`) instead of a heading so that the format is consistent across all documents.

```
**Step-by-step instructions:**

#. This is Step 1 with automatically numbered lines.

#. It is followed by Step 2.

   This line is indented by three spaces and will be correctly indented without a number.

#. This is Step 3.

#. This is Step 4.
```

For multi-level lists, separate indented sections with blank lines:

```
* First item in bullet list.
* Second item.

  * First sub-item of second item.
  * Second sub-item.

* Third item.
```

## Variable Definitions

Use this format for variable descriptions and definitions. Note definition lines are preceded by two (2) spaces. There must not be a blank line before the first line of the definition.

```
**Variable Name**
  This is the definition of variable name and is indented by two spaces.

  Multiline variable definition is preceded by one line and indented by two spaces.
```

## Cross References

Cross references can be to a target or topic file.

When creating and editing cross references, you may need to clean and rebuild the HTML for them to work properly.

### Cross Reference to a File

Use `:doc:` for a cross reference to a topic file to create a hyperlink that points to the beginning of the file.

To use the file's title as the hyperlink text:

```
This is a reference to :doc:`path/to/filename`
```

To use custom hyperlink text:

```
This is a reference to :doc:`hyperlink text <path/to/filename>`
```

Do not include the `.rst` extension in the filename.

Include a path to reference a file in a different folder.

For example, a reference to the `create_project` file in the `getting-started` folder:

* From another file in the `getting-started` folder, use only the file name (without the extension): `create_project`.

* From a file in a different folder, include the path: `../getting-started/create_project`.

### Cross Reference to a Target

Use `:ref:` for a cross reference to a target to create a hyperlink that points to the target in a topic file.

A target is defined like this. It should be followed by a blank line and precede a section heading:

```
Text for previous topic. Blah blah blah.

.. _pvavailability:

System Availability
~~~~~~~~~~~~~~~~~~~

This is text about PV system availability.
```

The target does not require a path. Each target in the entire project must be unique.

A reference to that target automatically inserts the heading "System Availability" with a hyperlink to that place in the document:

```
This is a reference to a target :ref:`pvavailability`
```

Or, you can set the hyperlink text to "PV System Availability" like this:

```
This is a reference to a target :ref:`PV System Availability <pvavailability>`
```
## Web Links

Web links are follwed by **two (2) underscores (__)**.

URL as link text:

```
For links to legacy datasets, see `<https://nsrdb.nrel.gov/data-sets/archives.html>`__.
```

Custom link text examples:

```
MacAlpine, S.; Deline, C. (2015) Simplified Method for Modeling the Impact of Arbitrary Partial Shading Conditions on PV Array Performance. National Renewable Energy Laboratory. 8 pp.; NREL/CP-5J00-64570. (`PDF 699 KB <http://www.nrel.gov/docs/fy15osti/64570.pdf>`__)
```

```
For more information about SAM, see the `SAM website <https://sam.nrel.gov>`__.
```

## Indentation

Sphinx is very sensitive to spacing and indentation.

**Do not use tabs!** Sphinx counts number of spaces, so tabs will break documentation builds.

## Table of Contents

for `toctree`, lines following the directive should be preceded by three spaces:

```
.. toctree::
   :maxdepth: 1
   :caption: Getting Started and Reference

   introduction/index
   getting-started/index
   reference/index
   window-reference/index
   simulation-options/index
```

## Notes and Highlighted Text Boxes

Use Sphix "admonitions" to highlight text in boxes: https://sphinx-book-theme.readthedocs.io/en/stable/reference/kitchen-sink/admonitions.html.

Use the `note` directive for text in a box notes:

```
.. note:: This note will be rendered in a box with an information (i) icon and the word "Note" in a blue title bar.
```

Admonitions can have more than one line. Each subsequent line is indented by three spaces:

```
.. note:: First line of a multiline note.

   Second line of multiline note.

   Third line of multiline note.
```

When Help was originally converted from Help and Manual's XML format to ReStructured text, multi-line notes were exported as separate notes and rendered in separate text boxes. If you encounter several notes in a row, you can combine them into a single note.

Before:

```
.. note:: First note before combining.

.. note:: Second note before combining.

.. note:: Third note before combining.
```

After:

```
.. note:: First note after combining.

   Second note after combining is now Line 2 in a single text box. (This line is indented by three spaces.)

   Third note after combining is now Line 3 in a single text box. (This line is also indented by three spaces.)
```


Use the `admonition` directive to use a custom title instead of "Note:"

```
.. admonition:: Custom title goes here and will be rendered with a bell icon.

   First line in box.

   Second line in box.
```

You can also use `topic` to create a text box with a custom title. This can be useful to make an example stand out.

```
.. topic:: Value Mode Example
  
   An AC degradation rate of 1% for a system with a net annual AC output of 100,000 kWh in Year one results in annual output values of 100,000 kWh in year 1, 99,000 kWh in year 2, 98,010 kWh in year 3, 97,029.9 kWh in year 4, etc.
```

## Literal Blocks

A literal or code block is indented and preceded by `::`.

This results in "This is some code:" (colon is appended to end of line):

```
This is some code::

	One line of code.
	Another line of code.
	And so on.
```

This is just a literal block:

```
::
	First line.
	Second line.
	Third line.
```

## File Names and Paths

Use backticks (\`) to enclose file names or paths like `C:\SAM\2025.4.16`.

