<script>
document.addEventListener('DOMContentLoaded', function () {
  tippy('#navbarCollapse > ul.navbar-nav.navbar-nav-scroll.me-auto > li:nth-child(1)', {
    content: 'How citizens <strong>engage</strong> with AI systems',
    animation: 'scale',
    placement: 'bottom',
    delay: [500, 0]
  });
});
document.addEventListener('DOMContentLoaded', function () {
  tippy('#navbarCollapse > ul.navbar-nav.navbar-nav-scroll.me-auto > li:nth-child(2)', {
    content: 'How citizens <strong>think</strong> about AI systems',
    animation: 'perspective',
    placement: 'bottom',
    delay: [500, 0]
  });
});
document.addEventListener('DOMContentLoaded', function () {
  tippy('#navbarCollapse > ul.navbar-nav.navbar-nav-scroll.me-auto > li:nth-child(3)', {
    content: 'Learn about what citizens <b>value</b>',
    animation: 'scale',
    placement: 'bottom',
    delay: [500, 0]
  });
});
document.addEventListener('DOMContentLoaded', function () {
  tippy('#navbarCollapse > ul.navbar-nav.navbar-nav-scroll.ms-auto > li:nth-child(1)', {
    content: 'More Info',
    animation: 'scale',
    placement: 'bottom',
    delay: [500, 0]
  });
});
document.addEventListener('DOMContentLoaded', function () {
  tippy('#navbarCollapse > ul.navbar-nav.navbar-nav-scroll.ms-auto > li:nth-child(2)', {
    content: 'English',
    animation: 'scale',
    placement: 'bottom',
    delay: [500, 0]
  });
});
document.addEventListener('DOMContentLoaded', function () {
  tippy('#navbarCollapse > ul.navbar-nav.navbar-nav-scroll.ms-auto > li:nth-child(3)', {
    content: 'Nederlands',
    animation: 'scale',
    placement: 'bottom',
    delay: [500, 0]
  });
});
document.addEventListener('DOMContentLoaded', function () {
  tippy('#quarto-header > nav > div > div.navbar-brand-container', {
    content: 'Landing Page',
    animation: 'scale',
    placement: 'bottom',
    delay: [500, 0]
  });
});
document.addEventListener("DOMContentLoaded", function () {
  // Initialize custom-panel-tabset as a Bootstrap tabset
  const customTabset = document.querySelector('.custom-panel-tabset');
  if (customTabset) {
    const tabs = customTabset.querySelectorAll('ul.nav-tabs > li.nav-item > a.nav-link');
    tabs.forEach(tab => {
      tab.addEventListener('click', function (e) {
        e.preventDefault();
        const bsTab = new bootstrap.Tab(tab);
        bsTab.show();
      });
    });
    // Activate the first tab by default
    if (tabs.length > 0) {
      const firstTab = new bootstrap.Tab(tabs[0]);
      firstTab.show();
    }
  }
});
</script>
