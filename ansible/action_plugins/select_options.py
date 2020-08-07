#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
ActionModule definition for the Ansible prompt action plugin.

.. moduleauthor:: Purr Consuegra, adapthed from Andrew Vaughan's (hello@andrewvaughan.io) code in https://github.com/andrewvaughan/ansible-role-prompt
"""

__metaclass__ = type

import re
import sys
import subprocess

from collections import namedtuple
from ansible.plugins.action import ActionBase

VALID_PARAMS = ['variable','msg','options']
Parameters = namedtuple('Parameters', VALID_PARAMS)
rValidVariable = re.compile(r"^[A-Za-z0-9_]+$")
    
VALID_OPTION_PARAMS = ['name','description', 'default']
Option = namedtuple('Option', VALID_OPTION_PARAMS)

class ActionModule(ActionBase):
    """
    Prompts user with a multiple choice dialog

    .. class:: ActionModule
    """

    TRANSFERS_FILES = False

    def run(self, tmp=None, task_vars=None):
        """
        Perform the plugin task, prompting the user to choose options

        :kwarg tmp: the temporary directory to use if creating files
        :kwarg task_vars: any variables associated with the task

        :returns: a dictionary of results from the module

        .. function:: run([tmp=None, task_vars=None])
        """
        task_vars = task_vars or dict()

        result = super(ActionModule, self).run(tmp, task_vars)
        args = self._task.args

        try:
            parameters = parseArgs(args)
        except Exception as e:
            return self._fail(result, e.args[0])

        return self._prompt(result, parseArgs(args))


    def _prompt(self, result, parameters):
        """
        Prompts the user with a message and optionally asks for a response.

        :kwarg result: the base result dict to build on
        :kwarg msg: The parsed arguments 

        :returns: an updated dict response with success or failure

        .. function:: _prompt(result, Parameters)
        """
        program = ["dialog", "--nocancel", "--separate-output", "--checklist", parameters.msg, "22", "76", "16"]

        # Parse each item on the list
        for o in parameters.options:
            onOff = "on" if o.default else "off"
            program.extend([o.name, o.description, onOff])

        # Convert to terminal input temporarily
        oldin = sys.stdin
        sys.stdin = open("/dev/tty")

        command = subprocess.Popen(program, stderr=subprocess.PIPE)
        output, dialogResult = command.communicate()
        dialogResult = dialogResult.rstrip("\n")
        dialogResult = dialogResult.split("\n") if dialogResult != "" else []

        # Revert to previous setting
        sys.stdin = oldin

        if 'ansible_facts' not in result:
            result['ansible_facts'] = dict()
        result['ansible_facts'][parameters.variable] = dialogResult

        return result


    def _fail(self, result, message, *args):
        """
        Raise an Ansible exception with a given message.

        :kwarg result: the base result object to build on
        :kwarg message: the message to pass to the Ansible exception
        :kwarg args: an arbitrary number of arguments to replace in the message's formatting

        :returns: an updated dict response with the provided failure condition

        .. function:: _fail(result, message, *args)
        """
        if not isinstance(result, dict):
            raise TypeError("Invalid result provided. Expected dict, received %s." % type(result))

        if not isinstance(message, (str, unicode)):
            raise TypeError("Invalid message provided. Expected string, received '%s'." % type(message))

        if message == "":
            raise ValueError("Empty message provided. Requires failure message.")

        result['failed'] = True
        result['msg'] = message % (args)

        return result

def parseArgs(args):
    checkUnexpectedParameters(args, VALID_PARAMS)

    variable = validateArg(args, 'variable', (unicode, str), 'string')
    options = validateArg(args, 'options', list, 'list of options')
    msg = validateArg(args, 'msg', (unicode, str), 'string', default = 'Choose options(s)')

    if len(variable) == 0:
        raise ValueError("Parameter 'variable' must provide variable name.  Empty received.")
    # Check for illegal ansible characters
    if not rValidVariable.search(variable):
        raise ValueError("Invalid character in 'variable' parameter '%s'." % variable)

    return Parameters(
            options = map(parseOption, options),
            variable = variable, msg = msg
            )

def parseOption(option):
    if isinstance(option, (unicode, str)):
        return Option(option, option, True)

    checkUnexpectedParameters(option, VALID_OPTION_PARAMS)

    name = validateArg(option, 'name', (unicode, str), 'string')
    description = validateArg(args, 'description', (unicode, str), 'string', default = name)
    default = validateArg(args, 'default', bool, 'boolean', default = True)

    return Option(name, description, default)

def validateArg(args, key, validTypes, typeDescription, default = None):
    if key not in args:
        if default:
            return default
        raise KeyError("Required '%s' parameter missing." % key)
    if not isinstance(args[key], validTypes):
        raise ValueError(
                "Parameter '%s' should be a %s was %s" % 
                (key, (typeDescription or str(validTypes)), args[key].__class__.__name__)
                )
    return args[key]

def checkUnexpectedParameters(args, validParams):
    for arg in args:
        if arg not in validParams:
            return KeyError("Unexpected parameter '%s'" % arg)

