# Binary2Name
## Automatic detection for binary code unctionality

This project was devoloped by: [Carol Hanna](https://github.com/carolhanna01) and [Abdallah Yassin](https://github.com/AbdallahYassin),advoised by Dr. Gabi Nakibly during Project in Computer Security course - Technion . 

## Introduction:
The main motivation for this project is to be a helpful tool for researchers of binary code. While analyzing a large binary.
We get binary datasets as input and using angr, a symbolic analysis tool to get intermediate representation of the code. From there, came the most extensive step in the project which was to preprocess the intermediate code in preparation to be used as input to a neural network. We used a deep neural network adopted from [code2seq](https://github.com/tech-srl/code2seq), which is intended for the same goal but on source code as input instead of binaries.

Getting started:
=====================
### Requirements:
    -   python3
    -   rouge package, version 0.3.2
    -   TensorFlow, version 1.13 (pip install rouge==0.3.2)
    
### get best results Quickly:
we have uploaded our best models, with the preprocessed data. to run it automatically follow:
>   cd code2seq
>   continue_best_model.sh --dataset=<coreutils|coreutils_dpdk>

### Full preprocessing and training:
#### Extarct our Datasets: 
>   cd our_dataset/<desired dataset>
>   tar -xzf <dataset_name>.tar.gz
    
#### Preprocessing:
first, change the run_exps.sh file to run desired model (default it path with constraints).
>   run_exps.sh <pre-processed data name> <dataset name: coreutils_ds|dpdk_linux_ds|gnu_dataset>

#### code2seq training:
>   cd code2seq
>   ./train.sh <pre-processed data name>
