#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
ActionModule definition for the Ansible prompt action plugin.

.. moduleauthor:: Andrew Vaughan <hello@andrewvaughan.io>
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

def parseArgs(args):
    checkUnexpectedParameters(args, VALID_PARAMS)

    variable = validateArg(args, 'variable', str, 'string')
    options = validateArg(args, 'options', list, 'list of options')
    msg = validateArg(args, 'msg', str, 'string', default = 'Choose options(s)')

    # Check for illegal ansible characters
    if not rValidVariable.search(variable):
        raise ValueError("Invalid character in 'variable' parameter '%s'." % variable)

    return Parameters(
            options = options.map(parseOption),
            variable = variable, msg = msg
            )

def parseOption(option):
    if isinstance(option, str):
        return Option(option, option, True)

    checkUnexpectedParameters(option, VALID_OPTION_PARAMS)

    name = validateArg(option, 'name', str, 'string')
    description = validateArg(args, 'description', str, 'string', default = name)
    default = validateArg(args, 'default', bool, 'boolean', default = True)

    return Option(name, description, default)

def validateArg(args, key, validTypes, typeDescription, default = None):
    if key not in args:
        if default:
            return default
        raise KeyError("Required '%s' parameter missing." % key)
    if not isinstance(args[key], validTypes):
        raise ValueError("Parameter '%s' should be a %s" % (key, (typeDescription or str(validTypes))))

def checkUnexpectedParameters(args, validParams):
    for arg in args:
        if arg not in validParams:
            return KeyError("Unexpected parameter '%s'" % arg)


class ActionModule(ActionBase):
    """
    Prompts user with one-or-more messages and optionally waits for input for each.

    .. class:: ActionModule
    """

    TRANSFERS_FILES = False
    VALID_PARAMS = [
        'say', 'ask'
    ]


    def __init__(self, task, connection, play_context, loader, templar, shared_loader_obj):
        """
        Initialize the prompt Ansible plugin.

        .. function:: __init__(task, connection, play_context, loader, templar, shared_loader_obj)
        """
        super(ActionModule, self).__init__(task, connection, play_context, loader, templar, shared_loader_obj)

    def run(self, tmp=None, task_vars=None):
        """
        Perform the plugin task, prompting the user tho choose options

        :kwarg tmp: the temporary directory to use if creating files
        :kwarg task_vars: any variables associated with the task

        :returns: a dictionary of results from the module

        .. function:: run([tmp=None, task_vars=None])
        """
        task_vars = task_vars or dict()

        result = super(ActionModule, self).run(tmp, task_vars)
        args = self._task.args

        # Expect only the messages parameter
        if 'msg' not in args:
            return self._fail(result, "Required 'msg' parameter missing.")

        if len(args) != 1:
            return self._fail(result, "Expected single 'msg' parameter. Multiple parameters given.")

        return self._prompt(result, args['msg'])


    def _prompt(self, result, msg):
        """
        Prompts the user with a message and optionally asks for a response.

        :kwarg result: the base result dict to build on
        :kwarg msg: the message provided to parse (string, object, or list)

        :returns: an updated dict response with success or failure

        .. function:: _prompt(result, msg)
        """
        if not isinstance(msg, list):
            msg = [msg]

        if len(msg) == 0:
            return self._fail(result, "No message provided")

        # Parse each item on the list
        for m in msg:

            if m is not None and not isinstance(m, dict):
                m = str(m)

            # If no message is provided, fail
            if m is None or len(m) == 0:
                return self._fail(result, "No message provided")

            # If this is a set of key/value pairs, parse it
            for arg in m:
                if arg not in self.VALID_PARAMS:
                    return self._fail(result, "Unexpected parameter '%s'" % arg)

            # If this is a prompt, ask it as such
            if 'ask' in m:

                # Check for valid variable name
                if m['ask'] is None or str(m['ask']).strip() == "":
                    return self._fail(result, "Parameter 'ask' must provide variable name.  Empty received.")

                # Check for illegal ansible characters
                if not self.rValidVariable.search(m['ask']):
                    return self._fail(result, "Invalid character in 'ask' parameter '%s'.", m['ask'])

                # If no say is provided, just make it blank
                if 'say' not in m:
                    m['say'] = ""

                # Convert to terminal input temporarily
                oldin = sys.stdin
                sys.stdin = open("/dev/tty")

                # Present empty string if "say" not provided
                askstr = m['say']
                #var = raw_input(askstr)

                program = ["dialog", "--nocancel", "--separate-output", "--checklist", "Mitäh", "22", "76", "16", "kala", "kala", "on"]
                command = subprocess.Popen(program, stderr=subprocess.PIPE)
                o, e = command.communicate()

                var = e

                # Revert to previous setting
                sys.stdin = oldin

                if 'ansible_facts' not in result:
                    result['ansible_facts'] = dict()
                result['ansible_facts'][m['ask']] = var


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
