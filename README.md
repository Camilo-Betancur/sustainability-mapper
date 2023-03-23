![Banner](https://user-images.githubusercontent.com/111535472/227365086-c1d0de30-c040-4ce9-9d84-b5f2c26048f8.png)

SAPIENT is a Command Line Interface Application designed by the [**Stockholm Environment Institute (SEI)**](https://sei.org/) to help decision-makers and investors assess the level of alignment that a set of projects have with the **Sustainable Development Goals (SDGs)** or the **European Taxonomy for Sustainable Activities (EU Taxonomy)**. It reads the texts of a folder containing PDF files and creates tabular and graphical outputs on the documents' correlation with the SDGs and the EU Taxonomy.

## Citation

If you use SAPIENT, please cite us as:

> Lobos Alva, I.; Cárdenas Vélez, M.; & Betancur Jaramillo, J. C.; Hernández Orozco, E.; Maestre Másmela, D. (2022). SAPIENT - Sustainability mApper for Planning and InvestmENT. https://github.com/Camilo-Betancur/SAPIENT.

# Installation
### Installing R language and other requirements

First, you must install  in your computer and an IDE such as 
To use SAPIENT, you need to install [R language](https://www.r-project.org/) and an Integrated Development Environment (IDE) such as [RStudio](https://posit.co/products/open-source/rstudio/). Then, you can install the required libraries by running the following command in an R terminal:

```R
install.packages(c("caTools", "cli", "DBI", "ggplot2", "ggraph", "glue", "here", "igraph", "jsonlite", "pdftools", "randomForest", "readr", "RSQLite", "showtext", "SnowballC", "sysfonts", "tidygraph", "tidytext", "tidyverse", "tm"))
```

You can then clone or [download the SAPIENT repository from GitHub](https://github.com/Camilo-Betancur/SAPIENT/).

SAPIENT works best with **R 4.2.2** or newer and a machine with 8 GB RAM and 2100 MHz 6-core processor or better if you want to analyze many documents.

## Getting Started

When you download/clone the SAPIENT repository, you should get a folder containing the following:

![imagen](https://user-images.githubusercontent.com/111535472/227301279-2562f67b-a218-4016-a7bc-900a53add820.png)

First, you must **open the R Project file named "SAPIENT (_Sapient.RProj_)"**. A new RStudio window will open with the title "SAPIENT - main - RStudio" 

![imagen](https://user-images.githubusercontent.com/111535472/227301758-e4d40885-2a0a-4b2f-87bc-af2e4e2639d9.png).

Then, you must **go back to the SAPIENT main folder and open the file named "_app.R_"**.

![imagen](https://user-images.githubusercontent.com/111535472/227311212-ef078181-c152-48eb-baab-0754482a3083.png)

Now, your RStudio window should look something like this:

![imagen](https://user-images.githubusercontent.com/111535472/227311768-31c8efb2-bfe6-4bd3-b3d2-46da5d609d26.png)

To run the app, you must locate the text cursor at the beggining of the file and press Ctrl + INTRO or click on the "Run" icon in the RStudio interface.

![imagen](https://user-images.githubusercontent.com/111535472/227312170-0e952473-a487-4701-9e5b-928d1e85cdfa.png)

After running the script, you should get something like this:

![imagen](https://user-images.githubusercontent.com/111535472/227314317-af26a8d5-a233-4252-94ce-25eb1903917a.png)

However, as the application runs on the Console and Plots tabs, we suggest you to maximize both.

![imagen](https://user-images.githubusercontent.com/111535472/227314522-d3cf4509-248c-40d0-8871-6a86c03e471b.png)

## Using the App

### Creating the folder for the PDFs

First, you need to copy the PDF files into the SAPIENT directory. To do this, please go to the main directory and look for the **PDF** folder. Inside it, you must create a folder that will contain the PDF files you want to analyze. You will not be able to select which files of the folder to analyze, so we invite you to think about folders as individual analysis corpus. For our example, we will create a Folder named **_Test_**.

![imagen](https://user-images.githubusercontent.com/111535472/227322377-43e354a9-15b4-4897-8861-86bf1810a5b2.png)
![imagen](https://user-images.githubusercontent.com/111535472/227322725-0c8cdc1c-28fd-4e11-a84b-472689ee8926.png)

Note: SAPIENT will only be able to analyze the folders that are located in **_SAPIENT/PDF/_**. 

### Running the app

When you run the app, you will see a greeting and contextual information about SAPIENT. Also, you will see a message that indicates the status of the fundamental app directories and a menu for selecting the analysis mode.

The app will give you three options, from which you will have to select writing the option number and pressing INTRO.

- **Option 1:** Sustainable Development Goals (SDGs).
- **Option 2:** European Taxonomy for Sustainable Activities (EUT)
- **Option 9:** Credits.

![imagen](https://user-images.githubusercontent.com/111535472/227321170-e7b02d08-e670-49e8-925b-8c98a5a06f6e.png)

After choosing an analysis mode, the application will ask you to tell it where will it find the texts to map. They can be located in a folder with PDFs (see [Creating the folder for the PDFs](https://github.com/Camilo-Betancur/SAPIENT/edit/main/README.md#creating-the-folder-for-the-pdfs)) or retrieved from a previously-saved PDF extraction.

For this example, we will choose to **Start analysis from scratch**.

![imagen](https://user-images.githubusercontent.com/111535472/227325846-a6d0b7bc-c036-4819-aa17-c573843eee79.png)

The app will ask us for the name of the folder containing the projects, which is the one we created before inside **_SAPIENT/PDF/_**. After that, the app will extract the texts from the PDFs and will ask you if you want to save the extracted texts into a .json file. If you choose to save it, the resulting file will be saved into **_SAPIENT/Saves/_** and you will be able to use it in future analysis just by choosing the option **2) Read saved data** and writing the project name.

![imagen](https://user-images.githubusercontent.com/111535472/227326179-f6247501-6401-4f81-9245-d9733689c57e.png)

### Saving the tabular results

After that, the mapping will start, and soon you will be asked to save or discard the tabular results of the analysis. If you choose to save it, you will find them saved into **_SAPIENT/Output/[Project name]/[Analysis mode]/data_**. For our example, we will find the tabular data at: **_SAPIENT/Output/Test/SDGs/data_**.

![imagen](https://user-images.githubusercontent.com/111535472/227328094-84983d1e-28a5-46f0-9142-8d0b78292a0c.png) 

![imagen](https://user-images.githubusercontent.com/111535472/227328481-21bd60b3-8dc8-4cb2-8165-b8f96d407e69.png)

### Viewing and saving the graphical outputs

After saving or discarding the tabular data, SAPIENT will prompt you to view the results plots. After pressing ENTER, the first plot will be displayed into the plots tab at the right side of RStudio. Also, SAPIENT will ask you to save or discard the plot which, if saved, can be found in **_SAPIENT/Output/[Project name]/[Analysis mode]/img_**. For our example, the path would be: **_SAPIENT/Output/Test/SDGs/img_**.

![imagen](https://user-images.githubusercontent.com/111535472/227330831-8973c0f5-288c-494a-a03c-e2a06a1376f3.png)

## Contact

For further information, please contact:
- Ivonne Lobos Alva ([ivonne.lobos@sei.org](mailto:ivonne.lobos@sei.org)),
- Mario Cárdenas ([mario.cardenas@sei.org](mailto:mario.cardenas@sei.org)),
- or Juan Camilo Betancur ([juan.betancur@sei.org](mailto:juan.betancur@sei.org)).
