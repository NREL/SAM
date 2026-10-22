Keyboard Shortcuts
==================

The keyboard shortcuts provide access to standard functions that are available from the :doc:`File menu <file_menu>` or the :doc:`Case menu <case_menu>` and special functions that are only accessible via a shortcut.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Shortcut
     - Function
     - Menu Command
   * - F1
     - Open Help system.
     - 
   * - F2
     - Rename case.
     - Case, Rename
   * - F5
     - Run a simulation for the current case.
     - Case, Simulate
   * - F6
     - Generate a PDF report for the current case.
     - Case, Generate report
   * - Shift-F4
     - Open the SAM log.
     - 
   * - Shift-F5
     - Start the code generator.
     - Case, Generate code
   * - Shift-F11
     - Run a simulation for all cases in the file.
     - 
   * - Shift-F12
     - Open inputs browser with inputs for current case.
     - 
   * - Ctrl-F5
     - Save case inputs as JSON.
     - 
   * - Ctrl-F6
     - Load case inputs from JSON file.
     - 
   * - Ctrl-F7
     - Save case inputs as JSON for SSC.
     - 
   * - Ctrl-F8
     - Load case inputs from JSON for SSC file.
     - 
   * - Ctrl-F9
     - Load case inputs from JSON for SSC file and run simulation.
     - 
   * - Ctrl-F10
     - Create a duplicate of the current case.
     - Case, Duplicate
   * - Ctrl-F11
     - Open the inputs browser with inputs for all cases in the file.
     - File, Inputs browser
   * - Ctrl-Shift-n
     - Open a new script editing window.
     - File, New script
   * - Ctrl-Shift-o
     - Open an .lk script file.
     - File, Open script
   * - Ctrl-o
     - Open a .sam file.
     - File, Open
   * - Ctrl-s
     - Save a .sam file.
     - File, Save
   * - Ctrl-w
     - Close the current SAM file.
     - File, Close

The following shortcuts are to special testing functions for software developers that may result in losing or changing data in your SAM file without a confirmation.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Shortcut
     - Function
   * - Ctrl-F1
     - Create a segmentation fault, which causes SAM to close without saving.
   * - Shift-F7
     - Open the SAM Development Environment (open source versions only).
   * - Shift-F8
     - Restart SAM and reset internal databases.
   * - Shift-F9
     - Open SAM AppData folder.

The following shortcut may cause SAM to stop functioning properly, so you should not use it unless you know what you are doing.

.. list-table::
   :width: 100%
   :align: center
   :header-rows: 1

   * - Shortcut
     - Function
   * - Shift-F10
     - Save inputs for current case as default values. This permanently modifies the original default files, so only do this if you have a reason to modify the defaults.

