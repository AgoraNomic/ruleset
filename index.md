---
title: Officer Template
layout: puremd
---

Hiya! This is the `gh-pages` branch of [`officer-template`](<https://github.com/AgoraNomic/officer-template>). This template uses Jekyll to build a website.

Here are some things to do to get started:

1. Update `_config.yml` by replacing `/officer-template` with your repository's name, e.g., `/Webmastor`.
2. **Optionally,** if you anticipate using your own CSS, create your `style.css` file and insert the following line where indicated in `_layouts/default.html` (replacing `YOUR-REPOSITORY-NAME` with same name you used in step 1.)

  ```html
  <link rel="stylesheet" href="https://agoranomic.org/YOUR-REPOSITORY-NAME/style.css">
  ```

3. Update the title in the prelude of this file (`Officer Template`) and change the contents to what you want people to see when they first open your microsite.
4. Add additional pages for more content. Be sure to link to them!

# What goes on my microsite?

If you aren't sure where to go with the site, here are some ideas:

- Introducing new players to your associated mechanics,
- Archiving historical reports,
- Providing a list of items you're tracking (rules, switches, documents, etc.), or
- Explaining your workflow to prospecting officers.
- Add some fun ASCII art.

# What are all these files/Where does my content go?

In this template, we have:

- `.gitignore` tells Git which files shouldn't ever be uploaded to GitHub. These are generally build artifacts.
- `_includes/` holds widgets and segments, such as the navigation bar or chat box.
- `_layouts/` holds layouts used to format content.
- `_config.yml` holds basic Jekyll configuration.
- `index.md` is the main page, accessible at <https://agoranomic.org/officer-template>.

Any files with a header such as the following will be processed by Jekyll:

```md
---
title: My Page
layout: default
---
```

All other files are made available via the website.

Here are some examples of file paths and their associated URLs:

- `index.md` is available at `agoranomic.org/officer-template`.
- `hello-world.md` is available at `agoranomic.org/officer-template/hello-world`
- `reports/2025-02-06.txt` is available at `agoranomic.org/officer-template/reports/2025-02-06.txt`

# Layouts

Two layouts are provided as standard:

- `puremd` should be used by most Markdown pages. This helps with positioning content on the default layout.
- `default` should be used by any pages that are already wrapped in a `<div class="content">`.

# Resources

Here are some resources to check out if you're curious about how anything works:

- [Git Guide](<https://github.com/git-guides>)
- [MDN Markdown Introduction](<https://developer.mozilla.org/en-US/docs/MDN/Writing_guidelines/Howto/Markdown_in_MDN>)
- [MDN HTML Tutorials, Guides, and References](<https://developer.mozilla.org/en-US/docs/Web/HTML>) for structuring content within your pages.
- [MDN CSS Tutorials, References, and Cookbook](<https://developer.mozilla.org/en-US/docs/Web/CSS>) for changing how HTML looks.
- [MDN JavaScript Tutorials, Guides, and References](<https://developer.mozilla.org/en-US/docs/Web/JavaScript>)
- [Jekyll Docs](<https://jekyllrb.com/docs/>)

Of course, feel free to reach out to your Webmastor if you have any questions or just want a hand with executing any ideas. 🙂
