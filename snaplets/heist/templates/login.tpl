<apply template="base">
      <h1>Login</h1>

      <p><loginError/></p>

      <form method="post" action="/login">
        <table id="info">
          <tr>
            <td>Login:</td><td><input type="text" name="login" size="20" /></td>
          </tr>
          <tr>
            <td>Password:</td><td><input type="password" name="password" size="20" /></td>
          </tr>
          <tr>
            <td></td>
            <td><input type="submit" value="Login" /></td>
          </tr>
        </table>
      </form>

      <p>Don't have a login yet? <a href="/new_user">Create a new user</a></p>
</apply>
