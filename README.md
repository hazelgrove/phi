# Motivation
As the Hazel developer community grows, it is important that new features are vetted and documented. We will be using a proposal process for new Hazel features, following the pattern in other language communities, e.g.

 * Python's PEP (https://www.python.org/dev/peps/pep-0001/)
 * Scala's SIP (https://docs.scala-lang.org/sips/)
 
At the moment, we are proceeding less formally than those processes. This will likely change as the system grows. Process suggestions are very welcome.

# Creating a Proposal for Hazel Improvement (PHI)
* Clone this repository.
* Determine the number and name for your PHI:
  * The number should be the next available number. To determine the next available number, look at:
  
    1. the existing directories in master; and
    2. the list of branches on GitHub.
    
    Make sure you are not using the same number as somebody else.

  * The name should be a dash-separated sequence of lowercase letters.
* We will be using the GitHub pull request system to review your proposal before it is accepted into master. Create a new branch named `#-name` where `#` is your PHI number and `name` is the name you chose.
* Create a new directory named `#-name`.
* Create a new file named `#-name.md` in that directory.
* Push your new branch to GitHub immediately so that there are not number choice clashes.
* Create a pull request for your branch. Mark it as a draft. We will use GitHub's PR features to provide feedback on the proposal as it develops.
* Make sure you are in the #hazel-phis channel on the Hazel Slack.

# Pronunciation
PHI is pronounced however you'd like to pronounce the greek letter "phi" (like "pie" or "fee").
