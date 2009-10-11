{% for keyword_id, edge_id in m.edge.o[id].subject %}
{{ m.rsc[keyword_id].title }}
{% if not forloop.last %},{% endif %}
{% endfor %}
