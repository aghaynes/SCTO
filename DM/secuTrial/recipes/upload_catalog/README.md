# secuTrial catalog upload 

Certain input data may already be available in a standardized form. In these cases a secuTrail catalog can be created to aid  in standardization of data entry. Examples where such catalogs are used are adresses or medicines. A small example catalog of medical products can be found [here](https://github.com/PatrickRWright/SCTO/blob/master/DM/secuTrial/data/med_catalog.csv).

Upload catalog:
1. FormBuilder Welcome page
2. Click "Catalogue list"
3. Click "Create new catalogue"
4. "Browse..." (e.g. [med_catalog.csv](https://github.com/PatrickRWright/SCTO/blob/master/DM/secuTrial/data/med_catalog.csv))
5. Select encoding (e.g. UTF-8) and "Upload"
6. In the following window check entries and make adjustments to the text if needed. "Save and back"

![editcatalog](https://github.com/PatrickRWright/SCTO/blob/master/DM/secuTrial/recipes/upload_catalog/fig/edit_catalog.png)

Include catalog in eCRF:
1. Within a Form in the FormBuilder create "New question"
2. Type is IASCatalogGroup
3. Select your newly uploaded catalog (e.g. med_catalog) and "Save"

4. In the CatalogGroup create new items for your catalog entries (e.g. product and family). See below screenshot for family.

5. In DataCapture, your eCRF should now show the following. You can click "Catalog" and a pop-up opens where a selection can be made.

