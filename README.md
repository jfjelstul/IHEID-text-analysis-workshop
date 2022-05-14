# Social Science Methods for Lawyers: Quantitative Text Analysis

**By Joshua C. Fjelstul, Ph.D.**

This workshop will provide an introduction to quantitative text analysis for legal research. First, we'll do a broad overview of text analysis methods and how they can be used to study international law. We'll do a survey of text analysis methods, identify the differences between supervised and unsupervised learning, and discuss how quantitative text analysis overlaps with statistics, machine learning (ML), and natural language processing (NLP). We'll talk about types of descriptive analysis, like frequency analysis and document similarity, and inferential analysis, like scaling and classification. 

Second, we'll learn some introductory text analysis methods for scaling and classifying legal documents, focusing on the intuition behind the methods and how to interpret the results. We'll cover unsupervised topic models for document classification (Latent Dirichlet allocation) and unsupervised methods for document scaling (Wordfish).

Third, we'll learn how to clean and manipulate text data in R to get it ready for analysis. We'll learn how to write regular expressions — a syntax for matching patterns in text data — and how to implement them in `R` using `stringr`. Regular expressions are critical for cleaning and manipulating text data, including making dummy variables that capture the presence or absence of certain content in legal documents. We'll use [this tutorial](https://jfjelstul.github.io/regular-expressions-tutorial/).

Fourth, we'll learn how to implement the methods we've discussed in R to answer a real research question using the text of judgments from the Court of Justice of the European Union (CJEU). Using the full text of recent CJEU judgments, we'll extract the name of the judge-rapporteur (the judge who writes the judgment), classify the judgments using a topic model, scale the documents on the primary latent dimension and validate that dimension, and then make visualizations to communicate which judge-rapporteurs specialize in which policy areas and which are on the periphery of the latent dimension.
