# SAPIENT - Sustainability mApper for Planning and InvestmENT
SAPIENT is a mapper Tool for helping sustainability decision-making and investment.

Please, cite us as:

## Overview

Decision-makers and investors often have to appraise large volumes of projects, or report on their contributions to sustainability. We developed SAPIENT as a tool for assessing the level of alignment that a set of projects have with the **Sustainable Development Goals (SDGs)** or the **European Taxonomy for Sustainable Activities (EU Taxonomy)**. 

SAPIENT is a Command Line Interface Application designed to be used in R environments such as [RStudio](https://posit.co/products/open-source/rstudio/). It can read the texts of a folder containing PDF files and will create tabular and graphical outputs on the documents correlation with the **SDGs** and the **EU Taxonomy**. This tool breaks each document into phrases, detects if it is correlated to a particular **SDG Target** or **EU Taxonomy Objective**, and produces reports on the individual documents' alignment to the agendas, but also across the whole document set.

## Installation

### Installing R language and RStudio

First, you must install [R language](https://www.r-project.org/) in your computer and an IDE such as [RStudio](https://posit.co/products/open-source/rstudio/).

### Installing required libraries

You can install the most recent version of the required libraries using the following commands in an R terminal.

```R
install.packages("caTools")
install.packages("cli")
install.packages("DBI")
install.packages("ggplot2")
install.packages("ggraph")
install.packages("glue")
install.packages("here")
install.packages("igraph")
install.packages("jsonlite")
install.packages("pdftools")
install.packages("randomForest")
install.packages("readr")
install.packages("RSQLite")
install.packages("showtext")
install.packages("SnowballC")
install.packages("sysfonts")
install.packages("tidygraph")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("tm")
```

### Installing the app

SAPIENT does not need to be installed to work. You can clone or download the [GitHub repository](https://github.com/Camilo-Betancur/SAPIENT/) and thats it! If you installed the libraries and have their last 

### Requirements

#### OS and R version

This tool should be used with R 4.2.2 or more recent. It has been tested in computers running Windows 10 and 11 with 8 Gb RAM and 2100 Mhz 6-core processor, so we recommend using a machine with those characteristics or better if you want to analyze many documents.

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

![imagen](https://user-images.githubusercontent.com/111535472/227324361-8f858f0c-8b9a-4d52-9051-4caf659da312.png)

The app will ask us for the name of the folder containing the projects, which is the one we created before inside **_SAPIENT/PDF/_**.





## Contact

[Summary of the key points covered in the manual, along with encouragement to continue using the app]
