# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'System Advisor Model (SAM)'
copyright = '2026, National Laboratory of the Rockies'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/co nfiguration.html#general-configuration

extensions = ['sphinx_togglebutton']

templates_path = ['_templates']
exclude_patterns = ['archive']

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_book_theme'
html_static_path = ['_static']
html_css_files = ['custom.css',]

html_logo = 'images/SAM-logo-horizontal.png'
html_title = 'SAM Help'

html_theme_options = { 
    'collapse_navbar': False,
    'max_navbar_depth': 4, 
    'show_toc_level': 1 # controls secondary (right) sidebar
}

html_show_sphinx = False

#html_sidebars = {
#    '**': ['sbt-sidebar-nav.html']
#}
