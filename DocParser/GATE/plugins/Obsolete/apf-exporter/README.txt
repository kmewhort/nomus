The APF exporter is a PR enabling the export of GATE-style annotations into 
the APF (Ace Pilot Format) required for ACE, in order to make user of the ACE
evaluation tool, for example. Simply place the exporter at the end of an 
application and set the parameters appropriately. In some cases (depending 
on the particular version of ACE tools) there are some attributes such as 
SOURCE which need to be added to the exported file, for the evaluation tools 
to work properly. If in doubt, use the default parameters.
