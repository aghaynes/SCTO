# secuTrial data import

In most cases data will be manually entered into the eCRF. However, in some cases
batch data needs to be imported. Data imports are specified in the FormBuilder.
This recipe specifies one way to set up data import into secuTrial. Other set ups
are also possible

An importable example project is available in: <br>
SCTO/secuTrial/data/proj_DEM00_Dev_20180910-1701_BONE_MINERAL_DENSITY.zip

In the FormBuilder:
1. select "Edit import configurations"
2. "New import configuration"
3. configure your import according to the example below

![confimp](https://github.com/PatrickRWright/SCTO/blob/master/secuTrial/recipes/import_data/fig/config_import.png "confimp")

4. select the form for which you would like to perfom an import
5. select "Edit import formats"
6. "New import format"
7. configure your import format according to the example below

![impformat](https://github.com/PatrickRWright/SCTO/blob/master/secuTrial/recipes/import_data/fig/import_format.png "impformat")

Note: In this example we are not setting up "mapping entries". If you have coded data you are importing you will need to explicitly map the coding to the secuTrial values. Alternatively you can decode your data before importing it.


