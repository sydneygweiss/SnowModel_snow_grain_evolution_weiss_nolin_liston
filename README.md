# SnowModel_snow_grain_evolution_weiss_nolin_liston
Incorporation of 4 snow grain evolution models into SnowModel
This code is Glen Liston's original work, with the addition of 4 snow grain evolution models by Sydney G Weiss (sydneygweiss@gmail.com)
Please read instructions below. 

To People Interested in Using MicroMet/SnowModel:

- MicroMet and SnowModel are freely available for use in
non-commercial (i.e., research only) activities.  SnowTran-3D (a
component of SnowModel) is copyrighted by InterWorks Consulting and
its commercial use is strictly forbidden without obtaining a
license from me.

- If you do use MicroMet/SnowModel, I ask that you keep me informed
of any publications that arise from the work, and that you
appropriately acknowledge my contribution to your efforts.  How you
do this I leave up to you, whether it be in the form of
co-authorship or a note in the acknowledgements section of the
paper.

- I would like feedback from users on what is working and what is
not.  In this way we can work together to improve the models and
make them more generally applicable.  For example, there are
already plans to improve the precipitation scheme and add the
ability to account for temperature inversions.

- I am keeping an email list of current users who I will send
notes to about model updates, bug fixes, etc.  If you want to be
added/removed from the list, let me know.

- If you find others who are interested in using the models, please
put them in contact with me so I can discuss the appropriateness of
their application and direct them to the ftp site where the code is
located.  The ftp site is not publicly advertised, and I would
prefer that people using the site and models get them from me.

- Refereed papers describing the MicroMet preprocessor, MicroMet,
and SnowModel are included in the 'docs' directory of the code
distribution.

- The latest version of the code, etc., is available from my
anonymous ftp server:

  ftp://gliston.cira.colostate.edu/micromet_snowmodel/

I will always include a date stamp in the .zip file name so that
you will know which file contains the latest version.  You can use
the 'diffs' script in the code directory to compare versions, or
read the code_change_list.txt.

- The following are included in the code distribution:
  a) A summary of the general model data input requirements (met
     data, dem, veg distributions, etc.).  See the docs directory.

  b) A summary of the required data input formats.  See the docs
     directory.

  c) Example input datasets for a small Colorado simulation
     domain.  See the met and topo_veg directories.

  d) All of the model code.  See the code and micromet_preproc
     directories, and the snowmodel.par file.

  e) Instructions on how to compile and run the model.  See the
     docs directory.

  f) Example model outputs from the example run.  See the outputs
     directory and the snowmodel.par file.

  g) A collection of supplementary programs that may be of use to
     you (e.g., a stand-alone barnes interpolation program, a
     program to convert met data from sub-hourly to hourly, a
     program to convert met data from hourly to daily).  See the
     misc_programs directory.

- I suggest you look at everything, repeat my example simulation,
and compare your outputs with the outputs provided in this
distribution.  Then when everything is working, you will know what
is required to set up (inputs, configuration, etc.) and run the
model for your domain/application.


Good Luck!
Glen

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Glen E. Liston, Ph.D.              | Email: 
Cooperative Institute for Research |   glen.liston@colostate.edu
  in the Atmosphere (CIRA)         |
Colorado State University          | Voice: (970) 491-8220
Fort Collins, Colorado 80523-1375  | FAX: (970) 491-8241
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


To run the example simulation you have to put the values in
snowmodel.par.example in the new snowmodel.par file.


