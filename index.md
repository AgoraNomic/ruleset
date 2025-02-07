---
title: Officer Template
layout: default
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

# Structure

In this template, we have:

- `_includes/` holds widgets and segments, such as the navigation bar or chat box.
- `_layouts/` holds layouts used to format content.
- `_config.yml` holds basic Jekyll configuration.
- `index.md` is the main page, accessible at <https://agoranomic.org/officer-template>.

All other `.md` and `.html` files are accessible at `.../file-base-name`. You may put files in folders to further organize. Any files with a header such as the following will be processed by Jekyll:

```md
---
title: My Page
layout: default
---
```

# Resources

Here are some resources to check out if you're curious about how anything works:

- [Git Guide](<https://github.com/git-guides>)
- [MDN Markdown Introduction](<https://developer.mozilla.org/en-US/docs/MDN/Writing_guidelines/Howto/Markdown_in_MDN>)
- [MDN HTML Tutorials, Guides, and References](<https://developer.mozilla.org/en-US/docs/Web/HTML>) for structuring content within your pages.
- [MDN CSS Tutorials, References, and Cookbook](<https://developer.mozilla.org/en-US/docs/Web/CSS>) for changing how HTML looks.
- [MDN JavaScript Tutorials, Guides, and References](<https://developer.mozilla.org/en-US/docs/Web/JavaScript>)
- [Jekyll Docs](<https://jekyllrb.com/docs/>)

Of course, feel free to reach out to your Webmastor if you have any questions or just want a hand with executing any ideas. :smile:
