#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
ActionModule definition for the Ansible prompt action plugin.

.. moduleauthor:: Purr Consuegra, adapthed from Andrew Vaughan's (hello@andrewvaughan.io) code in https://github.com/andrewvaughan/ansible-role-prompt
"""

__metaclass__ = type

import re
import sys

from collections import namedtuple
from ansible.plugins.action import ActionBase
from questionnaire import Questionnaire

usingPython3 = sys.version_info[0] >= 3
if usingPython3:
    unicode = str

stringType = (unicode, str)
basicString = dict(validTypes = stringType, description = 'string')

def defineType(typename, **defs):
    typeObj = namedtuple(typename, defs.keys())
    def parseType(args):
        return parse(typeObj, defs, args)

    return typeObj, parseType

Parameters, parseParameters= defineType('Parameters',
        questions = dict(validTypes = list, description = 'list of questions', mapper = lambda qs: list(map(parseQuestion, qs))),
        )

def checkValidVariable(variable):
    if len(variable) == 0:
        raise ValueError("Parameter 'variable' must provide variable name.  Empty received.")
    # Check for illegal ansible characters
    if not validVariableName.match(variable):
        raise ValueError("Invalid character in 'variable' parameter '%s'." % variable)
    return variable
validVariableName = re.compile(r"^[A-Za-z0-9_]+$")

Question, parseQuestion = defineType('Question',
        variable = dict(validTypes = stringType, description = 'variable name', mapper = checkValidVariable),
        question = basicString,
        many = dict(validTypes = bool, default = True),
        choices = dict(validTypes = list, mapper = lambda chs: list(map(parseChoice, chs))),
        )

Choice, parseFullChoice = defineType('Choice',
        value = basicString,
        description = dict(optional = True, **basicString),
        questions = dict(validTypes = list, optional = True, default = [], mapper = lambda qs: list(map(parseQuestion, qs)))
        )
def parseChoice(choice):
    if isinstance(choice, stringType):
        return Choice(value = choice, description = choice, questions = [])
    if not 'description' in choice:
        choice['description'] = choice['value']
    return parseFullChoice(choice)


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
            parameters = parseParameters(args)
        except Exception as e:
            return self._fail(result, e.args[0])

        return self._prompt(result, parameters)


    def _prompt(self, result, parameters):
        """
        Prompts the user with a message and optionally asks for a response.

        :kwarg result: the base result dict to build on
        :kwarg msg: The parsed arguments

        :returns: an updated dict response with success or failure

        .. function:: _prompt(result, Parameters)
        """

        # Convert to terminal input temporarily
        oldin = sys.stdin
        sys.stdin = open("/dev/tty")

        #print(parameters)

        q = Questionnaire()
        for question in parameters.questions:
            self.addQuestion(q, question)
        dialogResult = q.run()

        # Revert to previous setting
        sys.stdin = oldin

        if 'ansible_facts' not in result:
            result['ansible_facts'] = dict()
        for key in dialogResult:
            result['ansible_facts'][key] = unicode(dialogResult[key])

        return result
    
    def addQuestion(self, q, question, condition = None):
        promptType = 'many' if question.many else 'one'
        choices = [(c.value, c.description) for c in question.choices]

        quest = q.add(question.variable, *choices, prompt = question.question, prompter = promptType)
        if condition:
            quest.condition(condition)

        for choice in question.choices:
            for subQuestion in choice.questions:
                op = '=='
                if question.many:
                    op = lambda answers, value: unicode(value) in answers
                self.addQuestion(q, subQuestion, (question.variable, choice.value, op))

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

def parse(Constructor, definitions, args):
    checkUnexpectedParameters(args, definitions)
    parsed = {}
    for key in definitions:
        parsed[key] = validateArg(args, key, **definitions[key])

    return Constructor(**parsed)

def parseOption(option):
    if isinstance(option, (unicode, str)):
        return Option(option, option, True)

    checkUnexpectedParameters(option, VALID_OPTION_PARAMS)

    name = validateArg(option, 'name', (unicode, str), 'string')
    description = validateArg(option, 'description', (unicode, str), 'string', default = name)

    return Option(name, description)

def checkUnexpectedParameters(args, validParams):
    for arg in args:
        if arg not in validParams:
            return KeyError("Unexpected parameter '%s'" % arg)

def validateArg(args, key, validTypes, description = None, optional = False, default = None, isValid = None, mapper = None):
    if key not in args:
        if default:
            return default
        if not optional:
            raise KeyError("Required '%s' parameter missing." % key)
    if not isinstance(args[key], validTypes):
        raise ValueError(
                "Parameter '%s' should be a %s was %s" % 
                (key, (description or str(validTypes)), args[key].__class__.__name__)
                )
    if mapper is not None:
        return mapper(args[key])
    return args[key]
