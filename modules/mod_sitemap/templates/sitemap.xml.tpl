<?xml version="1.0" encoding="UTF-8"?>
<urlset
      xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9
            http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd">

{% cache 3600 sitemap_xml cat="text" cat="event" cat="location" cat="collection" %}
	{% with m.config.site.hostname.value|default:"localhost" as hostname %}
		{% for id in m.search[{latest cat="text" cat="event" cat="location" cat="collection" pagelen=20000}] %}
			{% with m.rsc[id].page_url as page_url %}
			<url>
			  <loc>http://{{ hostname }}{{ page_url|escapexml }}</loc>
			  <lastmod>{{ m.rsc[id].modified|date:"c" }}</lastmod>
			  <changefreq>daily</changefreq>
			  <priority>{% ifequal page_url "/" %}1.00{% else %}{% if m.rsc[id].page_path %}0.8{% else %}0.5{% endif %}{% endifequal %}</priority>
			</url>
			{% endwith %}
		{% endfor %}
	{% endwith %}
{% endcache %}

</urlset>
