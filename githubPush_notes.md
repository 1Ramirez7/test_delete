
**#30 remove changes to yml code to be back to the normal due to crash #30**
 -name of push in reference: fixed week error for ds350 on index file
 - after the push reference I started messing with the yamal code and it went south
 - so this push revernts the yml code back to what it was for this push: fixed week error for ds350 on index file
 
**#31 testing change from gh-pages to main under build and deployment  #31**
 - testing the setting on git hub settings/pages/branch/main
  - change from gh-pages to main
  
**#32 incorrect deployment fixed  #32**
 - the deployment only published my readme fileand not the index file with all links
 - I'm going to render site within r studio to see if that fixes. 
 - keeping github settings from push #31
 
**#33 reverting back to normal github push settings #33**
- reverting back to make sure everything still runs as the default template
- if succesful I'm going to save this copy as the default template
- then I'm going to try and switch to doc site and see if that works
- Important to note that I went back to gh-pages branch undoing push #31
- push #31 did not work out as it wasn't reading the htmls at all. 

**#34 one last try with ai advice #34**
- I'm going to use the publish and quarto yml code ai gave and see what that does 
- I never tried both codes together but this one I will try both. 
- This ai notes are in my normal portfolio repository notes
 - I will need to combine notes on this but I will try to just work with this template for now. 
 
**#35 reverting back to normal github push settings #35**
- did not work 
- this error: Error: No file in /home/runner/work/DS350-website-template/DS350-website-template matched to [**/requirements.txt or **/pyproject.toml], make sure you have checked out the target repository
- This file name is in the publish.yml but seems to be commmented out, and the libraries that will go in that file are in the publish.yml
- maybe who ever build the publish.yml was running into trouble with the requirements.txt file so just skip it and added to publish
  - Other research has shown the requirements.txt file so obviously it is a needed file, but idk why this defualt template has it comment out and obviously no requirement.txt file is present
  
**#36 Trying docs format #36**
 - deleteting the publish.yml file
 - edited quarto.yml and index.qmd to reflect the docs format
 - this was done by mirroring the time series set up
 - When doing this, need to render the site in R studio and then push to github
  - done under Build - Render Website in R studio
 - on the github settings/pages/branch/main/docs  - this is where the site is published and not template default of gh-pages/_site
  
**#37 Successful run w/ docs format #37**
- # 36 ran successfully for the doc format
 - delete the publish.yml file
 - delete the .github site_libs and _site folders
  - they were link to the gh-pages push

**#38 Python qmd files for ds250 #38**
- Erin's ds250 projects cause some errors because of the python chunk codes.
- my r studio hasnt been set up to run python code so I ran into a library error
- I'm going to figure out how to make a folder that has this libraries installed and stored
 - this step would probably have to be ran in vs code or where ever the python code was created first (r studio or vs code)
  - because when creating the python code, it has to be ran to complete, but to run it has to be properly loaded in the envoroment. 
- I added the requirements.txt to the quarto.yml and created the requirements.txt file 
 - I'm runnig this in vs code
- This is a major issue that needs to be address to make sure python and R code files are ran properly when repository is clone into a new divice


**#39 Successful run w/ python qmd files for ds250 #39**
- #38 ran successfully for the python qmd files
 - added the requirements.txt to the quarto.yml and created the requirements.txt file
 - ran the requirements.txt file in vs code
 - NEXT STEP IS TO CLONE ON A NEW DEVICE AND SEE HOW THE LIBRARY PROCESS WILL WORK. 
 - WHAT WILL HAPPEN WHEN FIRST RENDER ON vs CODE VS r STUDIO. 
 - The only thing change from my working template is the adding of the requirements.txt file, and nothing else changed.?

**#40 - deleted old gh-pages files, and added python requirements.txt file #40**

Push #36 on the template repository was successful with the doc format, so I will do the same here
Push #38 & 39 on the template repository was updating to handle DS250 course work (using python and VS code)
Was able to run Erin's ds250 code on my vs code
I'm not sure that fix the python library problem so I'm planning on testing to clone on a new device and see what happens
I may have to inlcude code steps to make sure users download python libraries before rendering site in vs or r studio
Idk how r studio will handle the python code though so it gets interesting for sure - Maybe first only rendering python files in vs code will probably be best, but this is like an extra step - Be nice to figure this out though
So this push updates to docs format and includes python's requirement.txt file

**#41 - removing unwanted files #41**

deleted some other files from gh-pages
also notice that when I pull, it pulls the gh-pages branch, so I have to switch back to main
I also notice github had another branch name gh-pages, so I deleted that branch
gonna do a run test to make sure everything runs find

**#42 - mkaing sure site is working #42**

testing to see everything is working fine after deleting some old gh-pages files and removing old gh-pages branch from github

**#44 - updating to template only & making raw copy #44**
   - This push should serve as a copy of a working template for the DS350 & DS250
     - This update handles the python code and the doc format, but I don't know how it does when clone to a new device and how it will run in vs code vs R studio.
       - Need to research how this yml, index, and requirements will be handle in vs code vs R studio in new devices
   - I should work on adding a file names for Ds350 and Ds250 so it can be share
     - This should allow for me to edit yml code or other independent files
        - should allow others to upload qmd files, and adding new yml code should not be a problem



**#45 updating readme.md file to have instructions #45**
- this push and probably next few will just be to update the readme.md file to have instructions.


