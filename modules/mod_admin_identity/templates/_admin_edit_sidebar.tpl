{% if m.rsc[id].is_a.person %}

{% sortable id="sort-user-credentials" %}

<div class="item-wrapper" id="sort-user-credentials">
	<h3 class="above-item clearfix do_blockminifier">
		<span class="title">Username/ password</span>
		<span class="arrow">make smaller</span>
	</h3>
	<div class="item clearfix admin-form">
		<p>Add/remove credentials. <a href="javascript:void(0)" class="do_dialog {title: 'Help about user credentials.', text: 'When you add credentials to a person then the person becomes an user. A person or machine can log on with those credentials and perform actions on your Zotonic system.<br/><br/>What an user can do depends on the groups the user is member of.', width: '450px'}">Need more help?</a></p>
		<p>
			{% if m.identity[id].is_user %}
				This person is an user.
			{% else %}
				This person is not yet an user.
			{% endif %}
			{% button action={dialog_set_username_password id=id} text="Set username/ password" %}
		</p>
	</div>
</div>

{% endif %}
