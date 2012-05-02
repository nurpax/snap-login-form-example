<apply template="base">
      <h1>Not logged in!</h1>

      <form method="post" action="/new_user">
        <table id="info">
          <tr>
            <td>Login:</td><td><input type="text" name="login" size="20" /></td>
          </tr>
          <tr>
            <td>Password:</td><td><input type="password" name="password" size="20" /></td>
          </tr>
          <tr>
            <td></td>
            <td><input type="submit" value="Add user" /></td>
          </tr>
        </table>

      </form>
</apply>
