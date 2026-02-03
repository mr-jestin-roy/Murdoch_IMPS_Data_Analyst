# Push this repository to GitHub

Your local repo is initialized and the initial commit is done.

**Note:** Plain `git push` does *not* create a repo on GitHub — the repo must exist first. You can create it on the website, or use the GitHub CLI (`gh`) to create it and push in one go (see Option A below).

---

## Option A: Create repo on GitHub and push in one command (recommended)

**GitHub CLI is installed.** From this project folder:

### 1. Log in to GitHub (one-time)

```bash
gh auth login
```

Follow the prompts (choose GitHub.com → HTTPS → authenticate in browser or with token).

### 2. Create the repo on GitHub and push

```bash
cd "/Users/jestinroy/Monash/Murdoch_IMPS project_Data_Analyst"

gh repo create Murdoch_IMPS_Data_Analyst --public --source=. --remote=origin --push
```

This creates **Murdoch_IMPS_Data_Analyst** on your GitHub account, sets `origin`, and pushes `main`. Use a different name if you prefer (e.g. `imps-party-data`).

---

## Option B: Create repo on the website, then push

1. Go to **https://github.com/new**
2. Set **Repository name** (e.g. `Murdoch_IMPS_Data_Analyst`), set **Public**, leave "Add a README" **unchecked**, click **Create repository**.
3. In your terminal:

```bash
cd "/Users/jestinroy/Monash/Murdoch_IMPS project_Data_Analyst"

git remote add origin https://github.com/YOUR_USERNAME/YOUR_REPO.git
git push -u origin main
```

After the first push, use `git push` for future updates.
