<ul id="sub-navigation">
{% for menu_item in menu_list %}
    <li><a href="{{ menu_item.uri }}">{{ menu_item.title }}</a></li>
{% endfor %}
</ul>