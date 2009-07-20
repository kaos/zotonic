{% wire id=#logon type="submit" postback="logon" delegate="z_auth" %}
<form id="{{ #logon }}" method="post" action="postback">
	<fieldset>
		<div class="form-item">
			<label for="zp-username">Name</label>
			<input type="text" name="zp-username" id="zp-username" />
		</div>
		<div class="form-item">
			<label for="zp-password">Password</label>
			<input type="password" name="zp-password" id="zp-password" />
		</div>
		<div class="form-item clearfix">
			{% button type="submit" text="Logon" %}
			{% button text="Cancel" action={redirect back} %}
		</div>
	</fieldset>
</form>
