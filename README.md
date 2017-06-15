# Handwritten Digit Identifier

# Description
The goal of this project is to create a program which will take an image of a handwritten digit, and identify the digit. The program should be able to preprocess the image into a matrix of values relating to the image, and be able to process this information to differentiate the character from its background. The program must then take this sanitized matrix and determine which character is being represented.

# How to use

Currently the project can only handle 32px by 32px PNG images, though could be expanded to handle different sizes later. The program must be trained each time it is opened, but aftwards can attempt to identify digits until it is closed.

# Requirements
* quicklisp
* opticl
