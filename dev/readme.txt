create a folder structure for shiny app
run load all r script

to connect existing project with remote repo
git add .
git commit -m "Initial project commit"
git remote add origin your origin
git branch -M main
git push -u origin main


create a new branch
git checkout -b my-feature
git add .
git commit -m "Add new data cleaning function"
# 1. Switch back to your main branch
git checkout main
git pull origin main
# 3. Merge your feature branch into main
git merge my-feature
git push origin main
git branch -d my-feature

